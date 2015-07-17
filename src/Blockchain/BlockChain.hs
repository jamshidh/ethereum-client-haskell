{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blockchain.BlockChain (
  nextDifficulty,
  addBlock,
  addBlocks,
  addTransaction,
  addTransactions,
  getBestBlock,
  getBestBlockHash,
  getGenesisBlockHash,
  runCodeForTransaction
  ) where

import Control.Monad
import Control.Monad.IfElse
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.State hiding (state)
import Data.Binary hiding (get)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock
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
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.Data.TransactionResult
import Blockchain.Database.MerklePatricia
import Blockchain.DB.CodeDB
import Blockchain.DB.ModifyStateDB
import Blockchain.DBM
import Blockchain.Constants
import Blockchain.ExtDBs
import Blockchain.Format
import Blockchain.Mining
import Blockchain.SHA
import Blockchain.Util
import Blockchain.VM
import Blockchain.VM.Code
import Blockchain.VM.OpcodePrices
import Blockchain.VM.VMState

--import Debug.Trace

{-
initializeBlockChain::ContextM ()
initializeBlockChain = do
  let bytes = rlpSerialize $ rlpEncode genesisBlock
  blockDBPut (BL.toStrict $ encode $ blockHash $ genesisBlock) bytes
  detailsDBPut "best" (BL.toStrict $ encode $ blockHash genesisBlock)
-}

nextDifficulty::Integer->UTCTime->UTCTime->Integer
nextDifficulty oldDifficulty oldTime newTime = max nextDiff' minimumDifficulty
    where
      nextDiff' = 
          if round (utcTimeToPOSIXSeconds newTime) >=
                 (round (utcTimeToPOSIXSeconds oldTime) + 8::Integer)
          then oldDifficulty - oldDifficulty `shiftR` 11
          else oldDifficulty + oldDifficulty `shiftR` 11

nextGasLimit::Integer->Integer->Integer
nextGasLimit oldGasLimit oldGasUsed = max (max 125000 3141592) ((oldGasLimit * 1023 + oldGasUsed *6 `quot` 5) `quot` 1024)

nextGasLimitDelta::Integer->Integer
nextGasLimitDelta oldGasLimit  = oldGasLimit `div` 1024

minGasLimit::Integer
minGasLimit = 125000

checkUnclesHash::Block->Bool
checkUnclesHash b = blockDataUnclesHash (blockBlockData b) == hash (rlpSerialize $ RLPArray (rlpEncode <$> blockBlockUncles b))

--data BlockValidityError = BlockDifficultyWrong Integer Integer | BlockNumberWrong Integer Integer | BlockGasLimitWrong Integer Integer | BlockNonceWrong | BlockUnclesHashWrong
{-
instance Format BlockValidityError where
    --format BlockOK = "Block is valid"
    format (BlockDifficultyWrong d expected) = "Block difficulty is wrong, is '" ++ show d ++ "', expected '" ++ show expected ++ "'"
-}

verifyStateRootExists::Block->ContextM Bool
verifyStateRootExists b = do
  val <- lift $ stateDBGet (BL.toStrict $ encode $ blockDataStateRoot $ blockBlockData b)
  case val of
    Nothing -> return False
    Just _ -> return True

checkParentChildValidity::(Monad m)=>Block->Block->m ()
checkParentChildValidity Block{blockBlockData=c} Block{blockBlockData=p} = do
    unless (blockDataDifficulty c == nextDifficulty (blockDataDifficulty p) (blockDataTimestamp p) (blockDataTimestamp c))
             $ fail $ "Block difficulty is wrong: got '" ++ show (blockDataDifficulty c) ++ "', expected '" ++ show (nextDifficulty (blockDataDifficulty p) (blockDataTimestamp p) (blockDataTimestamp c)) ++ "'"
    unless (blockDataNumber c == blockDataNumber p + 1) 
             $ fail $ "Block number is wrong: got '" ++ show (blockDataNumber c) ++ ", expected '" ++ show (blockDataNumber p + 1) ++ "'"
    unless (blockDataGasLimit c <= blockDataGasLimit p +  nextGasLimitDelta (blockDataGasLimit p))
             $ fail $ "Block gasLimit is too high: got '" ++ show (blockDataGasLimit c) ++ "', should be less than '" ++ show (blockDataGasLimit p +  nextGasLimitDelta (blockDataGasLimit p)) ++ "'"
    unless (blockDataGasLimit c >= blockDataGasLimit p - nextGasLimitDelta (blockDataGasLimit p))
             $ fail $ "Block gasLimit is too low: got '" ++ show (blockDataGasLimit c) ++ "', should be less than '" ++ show (blockDataGasLimit p -  nextGasLimitDelta (blockDataGasLimit p)) ++ "'"
    unless (blockDataGasLimit c >= minGasLimit)
             $ fail $ "Block gasLimit is lower than minGasLimit: got '" ++ show (blockDataGasLimit c) ++ "'"
    return ()

checkValidity::Monad m=>Block->ContextM (m ())
checkValidity b = do
  maybeParentBlock <- lift $ getBlock (blockDataParentHash $ blockBlockData b)
  case maybeParentBlock of
    Just parentBlock -> do
          checkParentChildValidity b parentBlock
          nIsValid <- nonceIsValid' b
          --unless nIsValid $ fail $ "Block nonce is wrong: " ++ format b
          unless (checkUnclesHash b) $ fail "Block unclesHash is wrong"
          stateRootExists <- verifyStateRootExists b
          unless stateRootExists $ fail ("Block stateRoot does not exist: " ++ show (pretty $ blockDataStateRoot $ blockBlockData b))
          return $ return ()
    Nothing -> fail ("Parent Block does not exist: " ++ show (pretty $ blockDataParentHash $ blockBlockData b))


{-
                    coinbase=prvKey2Address prvKey,
        stateRoot = SHA 0x9b109189563315bfeb13d4bfd841b129ff3fd5c85f228a8d9d8563b4dde8432e,
                    transactionsTrie = 0,
-}


runCodeForTransaction::Block->Integer->Address->Address->Transaction->ContextM (Either VMException B.ByteString, VMState)
runCodeForTransaction b availableGas tAddr newAddress ut | isContractCreationTX ut = do
  whenM isDebugEnabled $ liftIO $ putStrLn "runCodeForTransaction: ContractCreationTX"

  (result, vmState) <-
    create b 0 tAddr tAddr (transactionValue ut) (transactionGasPrice ut) availableGas newAddress (transactionInit ut)

  return (const B.empty <$> result, vmState)

runCodeForTransaction b availableGas tAddr owner ut | isMessageTX ut = do
  whenM isDebugEnabled $ liftIO $ putStrLn $ "runCodeForTransaction: MessageTX caller: " ++ show (pretty $ tAddr) ++ ", address: " ++ show (pretty $ transactionTo ut)

  call b 0 owner owner tAddr
          (fromIntegral $ transactionValue ut) (fromIntegral $ transactionGasPrice ut)
          (transactionData ut) (fromIntegral availableGas) tAddr





addBlocks::Bool->[Block]->ContextM ()
addBlocks isBeingCreated blocks = 
  forM_ blocks $ \block -> do
    before <- liftIO $ getPOSIXTime 
    addBlock isBeingCreated block
    after <- liftIO $ getPOSIXTime 

    liftIO $ putStrLn $ "#### Block insertion time = " ++ printf "%.4f" (realToFrac $ after - before::Double) ++ "s"


isNonceValid::Transaction->ContextM Bool
isNonceValid t = do
  case whoSignedThisTransaction t of
    Nothing -> return False --no nonce would work
    Just tAddr -> do
      addressState <- lift $ getAddressState tAddr
      return $ addressStateNonce addressState == transactionNonce t

codeOrDataLength::Transaction->Int
codeOrDataLength t | isMessageTX t = B.length $ transactionData t
codeOrDataLength t | isContractCreationTX t = codeLength $ transactionInit t

zeroBytesLength::Transaction->Int
zeroBytesLength t | isMessageTX t = length $ filter (==0) $ B.unpack $ transactionData t
zeroBytesLength t | isContractCreationTX t = length $ filter (==0) $ B.unpack codeBytes
                  where
                    Code codeBytes = transactionInit t

intrinsicGas::Transaction->Integer
intrinsicGas t = gTXDATAZERO * zeroLen + gTXDATANONZERO * (fromIntegral (codeOrDataLength t) - zeroLen) + gTX
    where
      zeroLen = fromIntegral $ zeroBytesLength t
--intrinsicGas t@ContractCreationTX{} = 5 * (fromIntegral (codeOrDataLength t)) + 500

addTransaction::Block->Integer->Transaction->EitherT String ContextM (VMState, Integer)
addTransaction b remainingBlockGas t = do
  let maybeAddr = whoSignedThisTransaction t

  case maybeAddr of
    Just x -> return ()
    Nothing -> left "malformed signature"

  let Just tAddr = maybeAddr
    
  nonceValid <- lift $ isNonceValid t



  let intrinsicGas' = intrinsicGas t
  whenM (lift isDebugEnabled) $
    liftIO $ do
      putStrLn $ "bytes cost: " ++ show (gTXDATAZERO * (fromIntegral $ zeroBytesLength t) + gTXDATANONZERO * (fromIntegral (codeOrDataLength t) - (fromIntegral $ zeroBytesLength t)))
      putStrLn $ "transaction cost: " ++ show gTX
      putStrLn $ "intrinsicGas: " ++ show (intrinsicGas')

  addressState <- lift $ lift $ getAddressState tAddr

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

  whenM (lift isDebugEnabled) $ liftIO $ putStrLn "running code"

  if success
      then do
        (result, newVMState') <- lift $ runCodeForTransaction b (transactionGasLimit t - intrinsicGas') tAddr theAddress t

        lift $ addToBalance (blockDataCoinbase $ blockBlockData b) (transactionGasLimit t * transactionGasPrice t)
        
        case result of
          Left e -> do
            whenM (lift isDebugEnabled) $ liftIO $ putStrLn $ CL.red $ show e
            return (newVMState'{vmException = Just e}, remainingBlockGas - transactionGasLimit t)
          Right x -> do
            let realRefund =
                  min (refund newVMState') ((transactionGasLimit t - vmGasRemaining newVMState') `div` 2)

            success <- lift $ pay "VM refund fees" (blockDataCoinbase $ blockBlockData b) tAddr ((realRefund + vmGasRemaining newVMState') * transactionGasPrice t)

            when (not success) $ error "oops, refund was too much"

            whenM (lift isDebugEnabled) $ liftIO $ putStrLn $ "Removing accounts in suicideList: " ++ intercalate ", " (show . pretty <$> suicideList newVMState')
            forM_ (suicideList newVMState') $ lift . lift . deleteAddressState


            return (newVMState', remainingBlockGas - (transactionGasLimit t - realRefund - vmGasRemaining newVMState'))
      else do
        lift $ addToBalance (blockDataCoinbase $ blockBlockData b) (intrinsicGas' * transactionGasPrice t)
        addressState <- lift $ lift $ getAddressState tAddr
        liftIO $ putStrLn $ "Insufficient funds to run the VM: need " ++ show (availableGas*transactionGasPrice t) ++ ", have " ++ show (addressStateBalance addressState)
        return (VMState{vmException=Just InsufficientFunds, vmGasRemaining=0, refund=0, debugCallCreates=Nothing, suicideList=[], logs=[], returnVal=Nothing}, remainingBlockGas)


printTransactionMessage::Transaction->ContextM ()
printTransactionMessage t = do
  case whoSignedThisTransaction t of
    Just tAddr -> do
      nonce <- lift $ fmap addressStateNonce $ getAddressState tAddr
      liftIO $ putStrLn $ CL.magenta "    =========================================================================="
      liftIO $ putStrLn $ CL.magenta "    | Adding transaction signed by: " ++ show (pretty tAddr) ++ CL.magenta " |"
      liftIO $ putStrLn $ CL.magenta "    |    " ++
        (
          if isMessageTX t
          then "MessageTX to " ++ show (pretty $ transactionTo t) ++ "              "
          else "Create Contract "  ++ show (pretty $ getNewAddress_unsafe tAddr nonce)
        ) ++ CL.magenta " |"
    _ -> liftIO $ putStrLn $ CL.red $ "Malformed Signature!"

formatAddress::Address->String
formatAddress (Address x) = BC.unpack $ B16.encode $ B.pack $ word160ToBytes x

addTransactions::Block->Integer->[Transaction]->ContextM ()
addTransactions _ _ [] = return ()
addTransactions b blockGas (t:rest) = do
  printTransactionMessage t

  before <- liftIO $ getPOSIXTime 

  stateRootBefore <- lift $ getStateRoot

  result <- runEitherT $ addTransaction b blockGas t

  let (resultString, response) =
          case result of 
            Left err -> (err, "")
            Right (state, _) -> ("Success!", BC.unpack $ B16.encode $ fromMaybe "" $ returnVal state)

  after <- liftIO $ getPOSIXTime 

  stateRootAfter <- lift $ getStateRoot

  mpdb <- fmap stateDB $ lift get

  addrDiff <- lift $ addrDbDiff mpdb stateRootBefore stateRootAfter

  detailsString <- getDebugMsg
  lift $ putTransactionResult $
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

  remainingBlockGas <-
    case result of
      Left e -> do
        liftIO $ putStrLn $ CL.red "Insertion of transaction failed!  " ++ e
        return blockGas
      Right (_, g') -> return g'

  addTransactions b remainingBlockGas rest
  
addBlock::Bool->Block->ContextM ()
addBlock isBeingCreated b@Block{blockBlockData=bd, blockBlockUncles=uncles} = do
  liftIO $ putStrLn $ "Inserting block #" ++ show (blockDataNumber bd) ++ " (" ++ show (pretty $ blockHash b) ++ ")."
  maybeParent <- lift $ getBlock $ blockDataParentHash bd
  case maybeParent of
    Nothing ->
      liftIO $ putStrLn $ "Missing parent block in addBlock: " ++ show (pretty $ blockDataParentHash bd) ++ "\n" ++
      "Block will not be added now, but will be requested and added later"
    Just parentBlock -> do
      lift $ setStateRoot $ blockDataStateRoot $ blockBlockData parentBlock
      let rewardBase = 1500 * finney
      addToBalance (blockDataCoinbase bd) rewardBase

      forM_ uncles $ \uncle -> do
        addToBalance (blockDataCoinbase bd) (rewardBase `quot` 32)
        addToBalance
          (blockDataCoinbase uncle)
          ((rewardBase*(8+blockDataNumber uncle - blockDataNumber bd )) `quot` 8)


      let transactions = blockReceiptTransactions b

      addTransactions b (blockDataGasLimit $ blockBlockData b) transactions

      dbs <- lift get

      b' <-
        if isBeingCreated
        then return b{blockBlockData = (blockBlockData b){blockDataStateRoot=stateRoot $ stateDB dbs}}
        else do
          when ((blockDataStateRoot (blockBlockData b) /= stateRoot (stateDB dbs))) $ do
            liftIO $ putStrLn $ "newStateRoot: " ++ show (pretty $ stateRoot $ stateDB dbs)
            error $ "stateRoot mismatch!!  New stateRoot doesn't match block stateRoot: " ++ show (pretty $ blockDataStateRoot $ blockBlockData b)
          return b

      valid <- checkValidity b'
      case valid of
        Right () -> return ()
        Left err -> error err
      -- let bytes = rlpSerialize $ rlpEncode b
      blkDataId <- lift $ putBlock b'
      replaceBestIfBetter (blkDataId, b')

getBestBlockHash::ContextM SHA
getBestBlockHash = do
  maybeBestHash <- lift $ detailsDBGet "best"
  case maybeBestHash of
    Nothing -> do
      bhSHA <- getGenesisBlockHash
      lift $ detailsDBPut "best" $ BL.toStrict $ encode bhSHA
      return bhSHA
    Just bestHash -> return $ decode $ BL.fromStrict $ bestHash

getGenesisBlockHash::ContextM SHA
getGenesisBlockHash = do
  maybeGenesisHash <- lift $ detailsDBGet "genesis"
  case maybeGenesisHash of
    Nothing -> do
      bhSHA <- blockHash <$> initializeGenesisBlock
      lift $ detailsDBPut "genesis" $ BL.toStrict $ encode bhSHA
      return bhSHA
    Just bestHash -> return $ decode $ BL.fromStrict $ bestHash

getBestBlock::ContextM Block
getBestBlock = do
  bestBlockHash <- getBestBlockHash
  bestBlock <- lift $ getBlock bestBlockHash
  return $ fromMaybe (error $ "Missing block in database: " ++ show (pretty bestBlockHash)) bestBlock
      

replaceBestIfBetter::(BlockDataRefId, Block)->ContextM ()
replaceBestIfBetter (blkDataId, b) = do
  best <- getBestBlock
  if blockDataNumber (blockBlockData best) >= n
    then return ()
    else do
    lift $ detailsDBPut "best" (BL.toStrict $ encode $ blockHash b)
    let oldStateRoot = blockDataStateRoot (blockBlockData best)
        newStateRoot = blockDataStateRoot (blockBlockData b)
    lift $ sqlDiff blkDataId n oldStateRoot newStateRoot
  where n = blockDataNumber (blockBlockData b)
