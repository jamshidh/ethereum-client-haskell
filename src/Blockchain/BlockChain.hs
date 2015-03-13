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
import Control.Monad.IO.Class
import Control.Monad.Trans
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

import qualified Blockchain.Colors as C
import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.DataDefs
import Blockchain.Data.BlockDB
import Blockchain.Data.Code
import Blockchain.Data.RLP
import Blockchain.Data.SignedTransaction
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia
import Blockchain.Debug
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
           --(round (utcTimeToPOSIXSeconds oldTime) + 5::Integer)
           (round (utcTimeToPOSIXSeconds oldTime) + 8::Integer)
      then oldDifficulty - oldDifficulty `shiftR` 10
      else oldDifficulty + oldDifficulty `shiftR` 10

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
    unless (blockDataDifficulty c == nextDifficulty (blockDataDifficulty p) (blockDataTimestamp p) ( blockDataTimestamp c))
             $ fail $ "Block difficulty is wrong: got '" ++ show (blockDataDifficulty c) ++ "', expected '" ++ show (nextDifficulty (blockDataDifficulty p) (blockDataTimestamp p) ( blockDataTimestamp c)) ++ "'"
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


runCodeForTransaction::Block->Integer->Address->Transaction->ContextM VMState
runCodeForTransaction b availableGas tAddr ut@ContractCreationTX{} = do
  when debug $ liftIO $ putStrLn "runCodeForTransaction: ContractCreationTX"

  --Create the new account
  let newAddress = getNewAddress tAddr $ tNonce ut
  lift $ putAddressState newAddress blankAddressState
  pay "transfer value1" tAddr newAddress (value ut)

  addressState <- lift $ getAddressState tAddr

  if availableGas*gasPrice ut < addressStateBalance addressState
    then do
    pay "pre-VM fees1" tAddr (blockDataCoinbase $ blockBlockData b) (availableGas*gasPrice ut)

    newVMStateOrException <- create b 0 tAddr tAddr (value ut) (gasPrice ut) availableGas newAddress (tInit ut)

    let newVMState =
            case newVMStateOrException of
              Left e -> (eState e){vmException = Just e}
              Right x -> x
      
    
-----------------

    when debug $ liftIO $ putStrLn $ "Removing accounts in suicideList: " ++ intercalate ", " (show . pretty <$> suicideList newVMState)
    forM_ (suicideList newVMState) $ \address -> do
      lift $ deleteAddressState address

    return newVMState
    else do
    liftIO $ putStrLn $ "Insufficient funds to run the VM: need " ++ show (availableGas*gasPrice ut) ++ ", have " ++ show (addressStateBalance addressState)
    return VMState{vmException=Just $ InsufficientFunds undefined, logs=[]}
    
runCodeForTransaction b availableGas tAddr ut@MessageTX{} = do
  when debug $ liftIO $ putStrLn $ "runCodeForTransaction: MessageTX caller: " ++ show (pretty $ tAddr) ++ ", address: " ++ show (pretty $ to ut)
  
  addressState <- lift $ getAddressState tAddr
  
  success <- pay "transfer value2" tAddr (to ut) (value ut)

  if not success
    then return VMState{vmException=Just $ InsufficientFunds undefined, logs=[]}
    else 
    if availableGas*gasPrice ut <= addressStateBalance addressState
    then do
      recipientAddressState <- lift $ getAddressState (to ut)
      contractCode <- lift $ fromMaybe B.empty <$> getCode (addressStateCodeHash recipientAddressState)

      pay "pre-VM fees2" tAddr (blockDataCoinbase $ blockBlockData b) (availableGas*gasPrice ut)

      newVMStateOrException <- runCodeForTransaction' b 0 tAddr tAddr (value ut) (gasPrice ut) availableGas (to ut) (Code contractCode) (tData ut)
-------------------------

      let newVMState =
            case newVMStateOrException of
              Left e -> (eState e){vmException = Just e}
              Right x -> x
      
      when debug $ liftIO $ putStrLn $ "Removing accounts in suicideList: " ++ intercalate ", " (show . pretty <$> suicideList newVMState)
      forM_ (suicideList newVMState) $ \address -> do
        lift $ deleteAddressState address

      return newVMState
    
    else do
      liftIO $ putStrLn "Insufficient funds to run the VM"
      return VMState{vmException=Just $ InsufficientFunds undefined, logs=[]}

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
intrinsicGas t = zeroLen + 5 * (fromIntegral (codeOrDataLength t) - zeroLen) + 500
    where
      zeroLen = fromIntegral $ zeroBytesLength t
--intrinsicGas t@ContractCreationTX{} = 5 * (fromIntegral (codeOrDataLength t)) + 500

addTransaction::Block->Integer->SignedTransaction->ContextM (VMState, Integer)
addTransaction b remainingBlockGas t@SignedTransaction{unsignedTransaction=ut} = do
  valid <- isTransactionValid t

  let signAddress = whoSignedThisTransaction t

  let intrinsicGas' = intrinsicGas ut
  when debug $
    liftIO $ putStrLn $ "intrinsicGas: " ++ show (intrinsicGas')

  addressState <- lift $ getAddressState signAddress

  if (tGasLimit ut * gasPrice ut + value ut <= addressStateBalance addressState) &&
     (intrinsicGas' <= tGasLimit ut) &&
     (tGasLimit ut <= remainingBlockGas) &&
     valid
    then do
    incrementNonce signAddress
    pay "intrinsic gas payment" signAddress (blockDataCoinbase $ blockBlockData b) (intrinsicGas' * gasPrice ut)
    when debug $ liftIO $ putStrLn "running code"

    let tAddr = whoSignedThisTransaction t

    newVMState <- runCodeForTransaction b (tGasLimit ut - intrinsicGas') tAddr ut

    when debug $
      liftIO $ putStrLn $ "gasRemaining: " ++ show (vmGasRemaining newVMState)

    --coinbaseAddressState <- lift $ getAddressState (coinbase $ blockData b)

    let realRefund =
          min (refund newVMState) ((tGasLimit ut - vmGasRemaining newVMState) `div` 2)

    when debug $ liftIO $ do
      putStrLn $ "full refund: " ++ show (refund newVMState)
      putStrLn $ "half spent: " ++ show ((tGasLimit ut - vmGasRemaining newVMState) `div` 2)
      putStrLn $ "realRefund: " ++ show realRefund

    when (isNothing . vmException $ newVMState) $ do
      success <- pay "VM refund fees" (blockDataCoinbase $ blockBlockData b) tAddr ((realRefund + vmGasRemaining newVMState) * gasPrice ut)
      when (not success) $ do
        --fail "coinbase doesn't have enough funds to refund the user"
        return ()
    return (newVMState, remainingBlockGas - (tGasLimit ut - realRefund - vmGasRemaining newVMState))
    else do
    when debug $ liftIO $ do
      putStrLn $ C.red "Insertion of transaction failed!"
      when (not $ tGasLimit ut * gasPrice ut + value ut <= addressStateBalance addressState) $ putStrLn "sender doesn't have high enough balance"
      when (not $ intrinsicGas' <= tGasLimit ut) $ putStrLn "intrinsic gas higher than transaction gas limit"
      when (not $ tGasLimit ut <= remainingBlockGas) $ putStrLn "block gas has run out"
      when (not valid) $ putStrLn "nonce incorrect"
    return
      (
        VMState{
           vmException=Just $ InsufficientFunds undefined,
           logs=[],
           newAccounts=[],
           vmGasRemaining=error "undefined vmGasRemaining",
           pc=error "undefined pc",
           memory=error "undefined memory"
           },
        remainingBlockGas
      )

    
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
  
  (_, remainingBlockGas) <- addTransaction b blockGas t

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
      lift $ putBlockSql b
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

