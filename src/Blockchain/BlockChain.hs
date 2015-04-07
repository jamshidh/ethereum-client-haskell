{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blockchain.BlockChain (
  nextDifficulty,
  addBlock,
  addBlocks,
  addTransaction,
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
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Blockchain.Colors as CL
import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.BlockDB
import Blockchain.Data.Code
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.Data.SignedTransaction
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia
import Blockchain.DB.CodeDB
import Blockchain.DB.ModifyStateDB
import Blockchain.DBM
import Blockchain.Constants
import Blockchain.ExtDBs
import Blockchain.Format
import Blockchain.Data.GenesisBlock
import Blockchain.SHA
import Blockchain.SigningTools
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
nextDifficulty oldDifficulty oldTime newTime =
    if round (utcTimeToPOSIXSeconds newTime) >=
           (round (utcTimeToPOSIXSeconds oldTime) + 8::Integer)
      then oldDifficulty - oldDifficulty `shiftR` 11
      else oldDifficulty + oldDifficulty `shiftR` 11

nextGasLimit::Integer->Integer->Integer
nextGasLimit oldGasLimit oldGasUsed = max 125000 ((oldGasLimit * 1023 + oldGasUsed *6 `quot` 5) `quot` 1024)

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
    unless (blockDataGasLimit c == nextGasLimit (blockDataGasLimit p) (blockDataGasUsed p))
             $ fail $ "Block gasLimit is wrong: got '" ++ show (blockDataGasLimit c) ++ "', expected '" ++ show (nextGasLimit (blockDataGasLimit p) (blockDataGasUsed p)) ++ "'"
    return ()

checkValidity::Monad m=>Block->ContextM (m ())
checkValidity b = do
  maybeParentBlock <- lift $ getBlock (blockDataParentHash $ blockBlockData b)
  case maybeParentBlock of
    Just parentBlock -> do
          checkParentChildValidity b parentBlock
          unless (nonceIsValid b) $ fail $ "Block nonce is wrong: " ++ format b
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
runCodeForTransaction b availableGas tAddr newAddress ut@ContractCreationTX{} = do
  whenM isDebugEnabled $ liftIO $ putStrLn "runCodeForTransaction: ContractCreationTX"

  (result, vmState) <-
    create b 0 tAddr tAddr (value ut) (gasPrice ut) availableGas newAddress (tInit ut)

  return (const B.empty <$> result, vmState)

runCodeForTransaction b availableGas tAddr owner ut@MessageTX{} = do
  whenM isDebugEnabled $ liftIO $ putStrLn $ "runCodeForTransaction: MessageTX caller: " ++ show (pretty $ tAddr) ++ ", address: " ++ show (pretty $ to ut)

  call b 0 owner owner tAddr
          (fromIntegral $ value ut) (fromIntegral $ gasPrice ut)
          (tData ut) (fromIntegral availableGas) tAddr





addBlocks::[Block]->ContextM ()
addBlocks blocks = 
  forM_ blocks addBlock

isTransactionValid::SignedTransaction->ContextM Bool
isTransactionValid t = do
  addressState <- lift $ getAddressState $ whoSignedThisTransaction t
  return (addressStateNonce addressState == tNonce (unsignedTransaction t))

codeOrDataLength::Transaction->Int
codeOrDataLength MessageTX{tData=d} = B.length d
codeOrDataLength ContractCreationTX{tInit=d} = codeLength d

zeroBytesLength::Transaction->Int
zeroBytesLength MessageTX{tData=d} = length $ filter (==0) $ B.unpack d
zeroBytesLength ContractCreationTX{tInit=Code d} = length $ filter (==0) $ B.unpack d

intrinsicGas::Transaction->Integer
intrinsicGas t = gTXDATAZERO * zeroLen + gTXDATANONZERO * (fromIntegral (codeOrDataLength t) - zeroLen) + gTX
    where
      zeroLen = fromIntegral $ zeroBytesLength t
--intrinsicGas t@ContractCreationTX{} = 5 * (fromIntegral (codeOrDataLength t)) + 500

addTransaction::Block->Integer->SignedTransaction->EitherT String ContextM (VMState, Integer)
addTransaction b remainingBlockGas t@SignedTransaction{unsignedTransaction=ut} = do
  valid <- lift $ isTransactionValid t

  let tAddr = whoSignedThisTransaction t

  let intrinsicGas' = intrinsicGas ut
  whenM (lift isDebugEnabled) $
    liftIO $ do
      putStrLn $ "bytes cost: " ++ show (gTXDATAZERO * (fromIntegral $ zeroBytesLength ut) + gTXDATANONZERO * (fromIntegral (codeOrDataLength ut) - (fromIntegral $ zeroBytesLength ut)))
      putStrLn $ "transaction cost: " ++ show gTX
      putStrLn $ "intrinsicGas: " ++ show (intrinsicGas')

  addressState <- lift $ lift $ getAddressState tAddr

  when (tGasLimit ut * gasPrice ut + value ut > addressStateBalance addressState) $ left "sender doesn't have high enough balance"
  when (intrinsicGas' > tGasLimit ut) $ left "intrinsic gas higher than transaction gas limit"
  when (tGasLimit ut > remainingBlockGas) $ left "block gas has run out"
  when (not valid) $ left "nonce incorrect"

  let availableGas = tGasLimit ut - intrinsicGas'    

  theAddress <-
    case ut of
      ContractCreationTX{} -> do
        lift $ getNewAddress tAddr
      MessageTX{} -> do
        lift $ incrementNonce tAddr
        return $ to ut
  
  success <- lift $ addToBalance tAddr (-tGasLimit ut * gasPrice ut)

  whenM (lift isDebugEnabled) $ liftIO $ putStrLn "running code"

  if success
      then do
        (result, newVMState') <- lift $ runCodeForTransaction b (tGasLimit ut - intrinsicGas') tAddr theAddress ut

        lift $ addToBalance (blockDataCoinbase $ blockBlockData b) (tGasLimit ut * gasPrice ut)
        
        case result of
          Left e -> do
            whenM (lift isDebugEnabled) $ liftIO $ putStrLn $ CL.red $ show e
            return (newVMState'{vmException = Just e}, remainingBlockGas - tGasLimit ut)
          Right x -> do
            let realRefund =
                  min (refund newVMState') ((tGasLimit ut - vmGasRemaining newVMState') `div` 2)

            success <- lift $ pay "VM refund fees" (blockDataCoinbase $ blockBlockData b) tAddr ((realRefund + vmGasRemaining newVMState') * gasPrice ut)

            when (not success) $ error "oops, refund was too much"

            whenM (lift isDebugEnabled) $ liftIO $ putStrLn $ "Removing accounts in suicideList: " ++ intercalate ", " (show . pretty <$> suicideList newVMState')
            forM_ (suicideList newVMState') $ lift . lift . deleteAddressState


            return (newVMState', remainingBlockGas - (tGasLimit ut - realRefund - vmGasRemaining newVMState'))
      else do
        lift $ addToBalance (blockDataCoinbase $ blockBlockData b) (intrinsicGas' * gasPrice ut)
        addressState <- lift $ lift $ getAddressState tAddr
        liftIO $ putStrLn $ "Insufficient funds to run the VM: need " ++ show (availableGas*gasPrice ut) ++ ", have " ++ show (addressStateBalance addressState)
        return (VMState{vmException=Just InsufficientFunds, vmGasRemaining=0, refund=0, debugCallCreates=Nothing, suicideList=[], logs=[], returnVal=Nothing}, remainingBlockGas)

      
addTransactions::Block->Integer->[SignedTransaction]->ContextM ()
addTransactions _ _ [] = return ()
addTransactions b blockGas (t@SignedTransaction{unsignedTransaction=ut}:rest) = do
  liftIO $ putStrLn "=========================================="
  liftIO $ putStrLn $ "Coinbase: " ++ show (pretty $ blockDataCoinbase $ blockBlockData b)
  liftIO $ putStrLn $ "Transaction signed by: " ++ show (pretty $ whoSignedThisTransaction t)
  addressState <- lift $ getAddressState $ whoSignedThisTransaction t
  liftIO $ do
    putStrLn $ "User balance: " ++ show (addressStateBalance $ addressState)
    putStrLn $ "tGasLimit ut: " ++ show (tGasLimit ut)
    putStrLn $ "blockGas: " ++ show blockGas
  
  result <- runEitherT $ addTransaction b blockGas t

  remainingBlockGas <-
    case result of
      Left e -> do
        liftIO $ putStrLn $ CL.red "Insertion of transaction failed!  " ++ e
        return blockGas
      Right (_, g') -> return g'

  liftIO $ putStrLn $ "remainingBlockGas: " ++ show remainingBlockGas

  addTransactions b remainingBlockGas rest
  
addBlock::Block->ContextM ()
addBlock b@Block{blockBlockData=bd, blockBlockUncles=uncles} = do
  liftIO $ putStrLn $ "Attempting to insert block #" ++ show (blockDataNumber bd) ++ " (" ++ show (pretty $ blockHash b) ++ ")."
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
                          addToBalance (blockDataCoinbase uncle) (rewardBase*15 `quot` 16)

      let transactions = blockReceiptTransactions b
      addTransactions b (blockDataGasLimit $ blockBlockData b) transactions

      dbs <- lift get
      liftIO $ putStrLn $ "newStateRoot: " ++ show (pretty $ stateRoot $ stateDB dbs)

      when (blockDataStateRoot (blockBlockData b) /= stateRoot (stateDB dbs)) $ do
        error $ "stateRoot mismatch!!  New stateRoot doesn't match block stateRoot: " ++ show (pretty $ blockDataStateRoot $ blockBlockData b)

      valid <- checkValidity b
      case valid of
        Right () -> return ()
        Left err -> error err
      let bytes = rlpSerialize $ rlpEncode b
      lift $ blockDBPut (BL.toStrict $ encode $ blockHash b) bytes
      replaceBestIfBetter b

getBestBlockHash::ContextM SHA
getBestBlockHash = do
  maybeBestHash <- lift $ detailsDBGet "best"
  case maybeBestHash of
    Nothing -> blockHash <$> initializeGenesisBlock
    Just bestHash -> return $ decode $ BL.fromStrict $ bestHash

getGenesisBlockHash::ContextM SHA
getGenesisBlockHash = do
  maybeGenesisHash <- lift $ detailsDBGet "genesis"
  case maybeGenesisHash of
    Nothing -> blockHash <$> initializeGenesisBlock
    Just bestHash -> return $ decode $ BL.fromStrict $ bestHash

getBestBlock::ContextM Block
getBestBlock = do
  bestBlockHash <- getBestBlockHash
  bestBlock <- lift $ getBlock bestBlockHash
  return $ fromMaybe (error $ "Missing block in database: " ++ show (pretty bestBlockHash)) bestBlock
      

replaceBestIfBetter::Block->ContextM ()
replaceBestIfBetter b = do
  best <- getBestBlock
  if blockDataNumber (blockBlockData best) >= blockDataNumber (blockBlockData b) 
       then return ()
       else lift $ detailsDBPut "best" (BL.toStrict $ encode $ blockHash b)

