{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blockchain.BlockChain (
  addBlock,
  addBlocks,
  addTransaction,
  addTransactions,
  runCodeForTransaction
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Binary hiding (get)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Printf

import qualified Blockchain.Colors as CL
import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.BlockDB
import Blockchain.Data.Code
import Blockchain.Data.DataDefs
import Blockchain.Data.DiffDB
import Blockchain.Data.GenesisBlock
import Blockchain.Data.Transaction
import Blockchain.Data.TransactionResult
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.ModifyStateDB
import Blockchain.DBM
import Blockchain.Constants
import Blockchain.ExtDBs
import Blockchain.ExtWord
import Blockchain.Options
import Blockchain.SHA
import Blockchain.Verifier
import Blockchain.VM
import Blockchain.VM.Code
import Blockchain.VM.OpcodePrices
import Blockchain.VM.VMState

--import Debug.Trace

addBlocks::Bool->[Block]->ContextM ()
addBlocks isBeingCreated blocks = 
  forM_ blocks $ \block -> do
    before <- liftIO $ getPOSIXTime 
    addBlock isBeingCreated block
    after <- liftIO $ getPOSIXTime 

    liftIO $ putStrLn $ "#### Block insertion time = " ++ printf "%.4f" (realToFrac $ after - before::Double) ++ "s"

addBlock::Bool->Block->ContextM ()
addBlock isBeingCreated b@Block{blockBlockData=bd, blockBlockUncles=uncles} = do
  liftIO $ putStrLn $ "Inserting block #" ++ show (blockDataNumber bd) ++ " (" ++ show (pretty $ blockHash b) ++ ")."
  maybeParent <- getBlock $ blockDataParentHash bd
  case maybeParent of
    Nothing ->
      liftIO $ putStrLn $ "Missing parent block in addBlock: " ++ show (pretty $ blockDataParentHash bd) ++ "\n" ++
      "Block will not be added now, but will be requested and added later"
    Just parentBlock -> do
      setStateDBStateRoot $ blockDataStateRoot $ blockBlockData parentBlock
      let rewardBase = 1500 * finney
      s1 <- addToBalance (blockDataCoinbase bd) rewardBase
      when (not s1) $ error "addToBalance failed even after a check in addBlock"

      forM_ uncles $ \uncle -> do
        s2 <- addToBalance (blockDataCoinbase bd) (rewardBase `quot` 32)
        when (not s2) $ error "addToBalance failed even after a check in addBlock"
        
        s3 <- addToBalance
              (blockDataCoinbase uncle)
              ((rewardBase*(8+blockDataNumber uncle - blockDataNumber bd )) `quot` 8)
        when (not s3) $ error "addToBalance failed even after a check in addBlock"


      let transactions = blockReceiptTransactions b

      addTransactions b (blockDataGasLimit $ blockBlockData b) transactions

      db <- getStateDB

      b' <-
        if isBeingCreated
        then return b{blockBlockData = (blockBlockData b){blockDataStateRoot=MP.stateRoot db}}
        else do
          when ((blockDataStateRoot (blockBlockData b) /= MP.stateRoot db)) $ do
            liftIO $ putStrLn $ "newStateRoot: " ++ show (pretty $ MP.stateRoot db)
            error $ "stateRoot mismatch!!  New stateRoot doesn't match block stateRoot: " ++ show (pretty $ blockDataStateRoot $ blockBlockData b)
          return b

      valid <- checkValidity b'
      case valid of
        Right () -> return ()
        Left err -> error err
      -- let bytes = rlpSerialize $ rlpEncode b
      blkDataId <- putBlock b'
      replaceBestIfBetter (blkDataId, b')

addTransactions::Block->Integer->[Transaction]->ContextM ()
addTransactions _ _ [] = return ()
addTransactions b blockGas (t:rest) = do

  result <-
    printTransactionMessage t b $
      runEitherT $ addTransaction b blockGas t

  remainingBlockGas <-
    case result of
      Left e -> do
          liftIO $ putStrLn $ CL.red "Insertion of transaction failed!  " ++ e
          return blockGas
      Right (_, g') -> return g'

  addTransactions b remainingBlockGas rest

addTransaction::Block->Integer->Transaction->EitherT String ContextM (VMState, Integer)
addTransaction b remainingBlockGas t = do
  tAddr <- whoSignedThisTransaction t ?! "malformed signature"

  nonceValid <- lift $ isNonceValid t

  let intrinsicGas' = intrinsicGas t
  when flags_debug $
    liftIO $ do
      putStrLn $ "bytes cost: " ++ show (gTXDATAZERO * (fromIntegral $ zeroBytesLength t) + gTXDATANONZERO * (fromIntegral (codeOrDataLength t) - (fromIntegral $ zeroBytesLength t)))
      putStrLn $ "transaction cost: " ++ show gTX
      putStrLn $ "intrinsicGas: " ++ show (intrinsicGas')

  addressState <- lift $ getAddressState tAddr

  when (transactionGasLimit t * transactionGasPrice t + transactionValue t > addressStateBalance addressState) $ left "sender doesn't have high enough balance"
  when (intrinsicGas' > transactionGasLimit t) $ left "intrinsic gas higher than transaction gas limit"
  when (transactionGasLimit t > remainingBlockGas) $ left "block gas has run out"
  when (not nonceValid) $ left "nonce incorrect"

  let availableGas = transactionGasLimit t - intrinsicGas'    

  theAddress <-
    if isContractCreationTX t
    then lift $ getNewAddress tAddr
    else do
      lift $ incrementNonce tAddr
      return $ transactionTo t
  
  success <- lift $ addToBalance tAddr (-transactionGasLimit t * transactionGasPrice t)

  when flags_debug $ liftIO $ putStrLn "running code"

  if success
      then do
        (result, newVMState') <- lift $ runCodeForTransaction b (transactionGasLimit t - intrinsicGas') tAddr theAddress t

        s1 <- lift $ addToBalance (blockDataCoinbase $ blockBlockData b) (transactionGasLimit t * transactionGasPrice t)
        when (not s1) $ error "addToBalance failed even after a check in addBlock"
        
        case result of
          Left e -> do
            when flags_debug $ liftIO $ putStrLn $ CL.red $ show e
            return (newVMState'{vmException = Just e}, remainingBlockGas - transactionGasLimit t)
          Right _ -> do
            let realRefund =
                  min (refund newVMState') ((transactionGasLimit t - vmGasRemaining newVMState') `div` 2)

            success' <- lift $ pay "VM refund fees" (blockDataCoinbase $ blockBlockData b) tAddr ((realRefund + vmGasRemaining newVMState') * transactionGasPrice t)

            when (not success') $ error "oops, refund was too much"

            when flags_debug $ liftIO $ putStrLn $ "Removing accounts in suicideList: " ++ intercalate ", " (show . pretty <$> suicideList newVMState')
            forM_ (suicideList newVMState') $ lift . deleteAddressState


            return (newVMState', remainingBlockGas - (transactionGasLimit t - realRefund - vmGasRemaining newVMState'))
      else do
        s1 <- lift $ addToBalance (blockDataCoinbase $ blockBlockData b) (intrinsicGas' * transactionGasPrice t)
        when (not s1) $ error "addToBalance failed even after a check in addTransaction"
        addressState' <- lift $ getAddressState tAddr
        liftIO $ putStrLn $ "Insufficient funds to run the VM: need " ++ show (availableGas*transactionGasPrice t) ++ ", have " ++ show (addressStateBalance addressState')
        return (VMState{vmException=Just InsufficientFunds, vmGasRemaining=0, refund=0, debugCallCreates=Nothing, suicideList=[], logs=[], returnVal=Nothing}, remainingBlockGas)

runCodeForTransaction::Block->Integer->Address->Address->Transaction->ContextM (Either VMException B.ByteString, VMState)
runCodeForTransaction b availableGas tAddr newAddress ut | isContractCreationTX ut = do
  when flags_debug $ liftIO $ putStrLn "runCodeForTransaction: ContractCreationTX"

  (result, vmState) <-
    create b 0 tAddr tAddr (transactionValue ut) (transactionGasPrice ut) availableGas newAddress (transactionInit ut)

  return (const B.empty <$> result, vmState)

runCodeForTransaction b availableGas tAddr owner ut = do --MessageTX
  when flags_debug $ liftIO $ putStrLn $ "runCodeForTransaction: MessageTX caller: " ++ show (pretty $ tAddr) ++ ", address: " ++ show (pretty $ transactionTo ut)

  call b 0 owner owner tAddr
          (fromIntegral $ transactionValue ut) (fromIntegral $ transactionGasPrice ut)
          (transactionData ut) (fromIntegral availableGas) tAddr

----------------


codeOrDataLength::Transaction->Int
codeOrDataLength t | isMessageTX t = B.length $ transactionData t
codeOrDataLength t = codeLength $ transactionInit t --is ContractCreationTX

zeroBytesLength::Transaction->Int
zeroBytesLength t | isMessageTX t = length $ filter (==0) $ B.unpack $ transactionData t
zeroBytesLength t = length $ filter (==0) $ B.unpack codeBytes' --is ContractCreationTX
                  where
                    Code codeBytes' = transactionInit t

intrinsicGas::Transaction->Integer
intrinsicGas t = gTXDATAZERO * zeroLen + gTXDATANONZERO * (fromIntegral (codeOrDataLength t) - zeroLen) + gTX
    where
      zeroLen = fromIntegral $ zeroBytesLength t
--intrinsicGas t@ContractCreationTX{} = 5 * (fromIntegral (codeOrDataLength t)) + 500


printTransactionMessage::Transaction->Block->ContextM (Either String (VMState, Integer))->ContextM (Either String (VMState, Integer))
printTransactionMessage t b f = do
  case whoSignedThisTransaction t of
    Just tAddr -> do
      nonce <- fmap addressStateNonce $ getAddressState tAddr
      liftIO $ putStrLn $ CL.magenta "    =========================================================================="
      liftIO $ putStrLn $ CL.magenta "    | Adding transaction signed by: " ++ show (pretty tAddr) ++ CL.magenta " |"
      liftIO $ putStrLn $ CL.magenta "    |    " ++
        (
          if isMessageTX t
          then "MessageTX to " ++ show (pretty $ transactionTo t) ++ "              "
          else "Create Contract "  ++ show (pretty $ getNewAddress_unsafe tAddr nonce)
        ) ++ CL.magenta " |"
    _ -> liftIO $ putStrLn $ CL.red $ "Malformed Signature!"

  stateRootBefore <- fmap MP.stateRoot getStateDB

  before <- liftIO $ getPOSIXTime 

  result <- f

  after <- liftIO $ getPOSIXTime 

  stateRootAfter <- fmap MP.stateRoot getStateDB

  mpdb <- getStateDB
  
  addrDiff <- addrDbDiff mpdb stateRootBefore stateRootAfter

  let (resultString, response) =
        case result of 
          Left err -> (err, "")
          Right (state', _) -> ("Success!", BC.unpack $ B16.encode $ fromMaybe "" $ returnVal state')

  detailsString <- getDebugMsg
  _ <-
        putTransactionResult $
        TransactionResult {
          transactionResultBlockHash=blockHash b,
          transactionResultTransactionHash=transactionHash t,
          transactionResultMessage=resultString,
          transactionResultResponse=response,
          transactionResultTrace=detailsString,
          transactionResultGasUsed=0,
          transactionResultEtherUsed=0,
          transactionResultContractsCreated=intercalate "," $ map formatAddress [x|CreateAddr x _ <- addrDiff],
          transactionResultContractsDeleted=intercalate "," $ map formatAddress [x|DeleteAddr x <- addrDiff],
          transactionResultTime=realToFrac $ after - before::Double,
          transactionResultNewStorage="",
          transactionResultDeletedStorage=""
          }
  

  clearDebugMsg

  liftIO $ putStrLn $ CL.magenta "    |" ++ " t = " ++ printf "%.2f" (realToFrac $ after - before::Double) ++ "s                                                              " ++ CL.magenta "|"
  liftIO $ putStrLn $ CL.magenta "    =========================================================================="

  return result






formatAddress::Address->String
formatAddress (Address x) = BC.unpack $ B16.encode $ B.pack $ word160ToBytes x

--Convert Maybe exception handling to EitherT exception
(?!)::Monad m=>
      Maybe a->err->EitherT err m a
x ?! err = maybe (left err) return $ x





----------------

replaceBestIfBetter::(BlockDataRefId, Block)->ContextM ()
replaceBestIfBetter (blkDataId, b) = do
  best <- getBestBlock
  if blockDataNumber (blockBlockData best) >= n
    then return ()
    else do
    detailsDBPut "best" (BL.toStrict $ encode $ blockHash b)
    let oldStateRoot = blockDataStateRoot (blockBlockData best)
        newStateRoot = blockDataStateRoot (blockBlockData b)
    sqlDiff blkDataId n oldStateRoot newStateRoot
  where n = blockDataNumber (blockBlockData b)
