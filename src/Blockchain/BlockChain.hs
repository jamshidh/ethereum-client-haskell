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
import Blockchain.Data.AddressState
import Blockchain.Data.Block
import Blockchain.Data.Code
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
checkUnclesHash b = unclesHash (blockData b) == hash (rlpSerialize $ RLPArray (rlpEncode <$> blockUncles b))

--data BlockValidityError = BlockDifficultyWrong Integer Integer | BlockNumberWrong Integer Integer | BlockGasLimitWrong Integer Integer | BlockNonceWrong | BlockUnclesHashWrong
{-
instance Format BlockValidityError where
    --format BlockOK = "Block is valid"
    format (BlockDifficultyWrong d expected) = "Block difficulty is wrong, is '" ++ show d ++ "', expected '" ++ show expected ++ "'"
-}

verifyStateRootExists::Block->ContextM Bool
verifyStateRootExists b = do
  val <- lift $ stateDBGet (BL.toStrict $ encode $ bStateRoot $ blockData b)
  case val of
    Nothing -> return False
    Just _ -> return True

checkParentChildValidity::(Monad m)=>Block->Block->m ()
checkParentChildValidity Block{blockData=c} Block{blockData=p} = do
    unless (difficulty c == nextDifficulty (difficulty p) (timestamp p) ( timestamp c))
             $ fail $ "Block difficulty is wrong: got '" ++ show (difficulty c) ++ "', expected '" ++ show (nextDifficulty (difficulty p) (timestamp p) ( timestamp c)) ++ "'"
    unless (number c == number p + 1) 
             $ fail $ "Block number is wrong: got '" ++ show (number c) ++ ", expected '" ++ show (number p + 1) ++ "'"
    unless (gasLimit c == nextGasLimit (gasLimit p) (gasUsed p))
             $ fail $ "Block gasLimit is wrong: got '" ++ show (gasLimit c) ++ "', expected '" ++ show (nextGasLimit (gasLimit p) (gasUsed p)) ++ "'"
    return ()

checkValidity::Monad m=>Block->ContextM (m ())
checkValidity b = do
  maybeParentBlock <- lift $ getBlock (parentHash $ blockData b)
  case maybeParentBlock of
    Just parentBlock -> do
          checkParentChildValidity b parentBlock
          unless (nonceIsValid b) $ fail $ "Block nonce is wrong: " ++ format b
          unless (checkUnclesHash b) $ fail "Block unclesHash is wrong"
          stateRootExists <- verifyStateRootExists b
          unless stateRootExists $ fail ("Block stateRoot does not exist: " ++ show (pretty $ bStateRoot $ blockData b))
          return $ return ()
    Nothing -> fail ("Parent Block does not exist: " ++ show (pretty $ parentHash $ blockData b))


{-
                    coinbase=prvKey2Address prvKey,
        stateRoot = SHA 0x9b109189563315bfeb13d4bfd841b129ff3fd5c85f228a8d9d8563b4dde8432e,
                    transactionsTrie = 0,
-}


runCodeForTransaction::Block->Integer->Address->Transaction->ContextM VMState
runCodeForTransaction b availableGas tAddr ut@ContractCreationTX{} = do
  whenM isDebugEnabled $ liftIO $ putStrLn "runCodeForTransaction: ContractCreationTX"

  --Create the new account
  let newAddress = getNewAddress tAddr $ tNonce ut
  lift $ putAddressState newAddress blankAddressState
  pay "transfer value1" tAddr newAddress (value ut)

  addressState <- lift $ getAddressState tAddr

  if availableGas*gasPrice ut < balance addressState
    then do
    pay "pre-VM fees1" tAddr (coinbase $ blockData b) (availableGas*gasPrice ut)

    (result, newVMState') <- create b 0 tAddr tAddr (value ut) (gasPrice ut) availableGas newAddress (tInit ut)

    newVMState <-
      case result of
        Left e -> do
          whenM isDebugEnabled $ liftIO $ putStrLn $ CL.red $ show e
          return newVMState'{vmException = Just e}
        Right x -> return newVMState'
      
    newAddressState <- lift $ getAddressState newAddress
    case returnVal newVMState of
      Just codeBytes ->
        lift $ putAddressState newAddress newAddressState{codeHash=hash codeBytes}
      Nothing -> return ()
    
-----------------

    

    return newVMState
    else do
    liftIO $ putStrLn $ "Insufficient funds to run the VM: need " ++ show (availableGas*gasPrice ut) ++ ", have " ++ show (balance addressState)
    return VMState{vmException=Just InsufficientFunds, vmGasRemaining=0, refund=0, debugCallCreates=Nothing, logs=[]}
    
runCodeForTransaction b availableGas tAddr ut@MessageTX{} = do
  whenM isDebugEnabled $ liftIO $ putStrLn $ "runCodeForTransaction: MessageTX caller: " ++ show (pretty $ tAddr) ++ ", address: " ++ show (pretty $ to ut)
  
  addressState <- lift $ getAddressState tAddr
  
  success <- pay "transfer value2" tAddr (to ut) (value ut)

  if not success
    then return VMState{vmException=Just InsufficientFunds, logs=[]}
    else 
    if availableGas*gasPrice ut <= balance addressState
    then do
      recipientAddressState <- lift $ getAddressState (to ut)
      contractCode <- lift $ fromMaybe B.empty <$> getCode (codeHash recipientAddressState)

      pay "pre-VM fees2" tAddr (coinbase $ blockData b) (availableGas*gasPrice ut)

      (result, newVMState') <- call b 0 (to ut) (to ut) tAddr
                               (fromIntegral $ value ut) (fromIntegral $ gasPrice ut) (tData ut) (fromIntegral availableGas) (to ut)
-------------------------


      
      newVMState <-
        case result of
          Left e -> do
            whenM isDebugEnabled $ liftIO $ putStrLn $ CL.red $ show e
            return newVMState'{vmException = Just e}
          Right x -> return newVMState'
      
      return newVMState
    
    else do
      liftIO $ putStrLn "Insufficient funds to run the VM"
      return VMState{vmException=Just InsufficientFunds, logs=[]}

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

addTransaction::Block->Integer->SignedTransaction->ContextM (VMState, Integer)
addTransaction b remainingBlockGas t@SignedTransaction{unsignedTransaction=ut} = do
  valid <- isTransactionValid t

  let signAddress = whoSignedThisTransaction t

  let intrinsicGas' = intrinsicGas ut
  whenM isDebugEnabled $
    liftIO $ putStrLn $ "intrinsicGas: " ++ show (intrinsicGas')

  addressState <- lift $ getAddressState signAddress

  if (tGasLimit ut * gasPrice ut + value ut <= balance addressState) &&
     (intrinsicGas' <= tGasLimit ut) &&
     (tGasLimit ut <= remainingBlockGas) &&
     valid
    then do
    incrementNonce signAddress
    pay "intrinsic gas payment" signAddress (coinbase $ blockData b) (intrinsicGas' * gasPrice ut)
    whenM isDebugEnabled $ liftIO $ putStrLn "running code"

    let tAddr = whoSignedThisTransaction t

    newVMState <- runCodeForTransaction b (tGasLimit ut - intrinsicGas') tAddr ut

    whenM isDebugEnabled $
      liftIO $ putStrLn $ "gasRemaining: " ++ show (vmGasRemaining newVMState)

    --coinbaseAddressState <- lift $ getAddressState (coinbase $ blockData b)

    let realRefund =
          min (refund newVMState) ((tGasLimit ut - vmGasRemaining newVMState) `div` 2)

    whenM isDebugEnabled $ liftIO $ do
      putStrLn $ "full refund: " ++ show (refund newVMState)
      putStrLn $ "half spent: " ++ show ((tGasLimit ut - vmGasRemaining newVMState) `div` 2)
      putStrLn $ "realRefund: " ++ show realRefund

    when (isNothing . vmException $ newVMState) $ do
      success <- pay "VM refund fees" (coinbase $ blockData b) tAddr ((realRefund + vmGasRemaining newVMState) * gasPrice ut)
      when (not success) $ do
        --fail "coinbase doesn't have enough funds to refund the user"
        return ()
    return (newVMState, remainingBlockGas - (tGasLimit ut - realRefund - vmGasRemaining newVMState))
    else do
    whenM isDebugEnabled $ liftIO $ do
      putStrLn $ CL.red "Insertion of transaction failed!"
      when (not $ tGasLimit ut * gasPrice ut + value ut <= balance addressState) $ putStrLn "sender doesn't have high enough balance"
      when (not $ intrinsicGas' <= tGasLimit ut) $ putStrLn "intrinsic gas higher than transaction gas limit"
      when (not $ tGasLimit ut <= remainingBlockGas) $ putStrLn "block gas has run out"
      when (not valid) $ putStrLn "nonce incorrect"
    return
      (
        VMState{
           vmException=Just InsufficientFunds,
           logs=[],
           debugCallCreates=Nothing,
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
  liftIO $ putStrLn $ "Coinbase: " ++ show (pretty $ coinbase $ blockData b)
  liftIO $ putStrLn $ "Transaction signed by: " ++ show (pretty $ whoSignedThisTransaction t)
  addressState <- lift $ getAddressState $ whoSignedThisTransaction t
  liftIO $ do
    putStrLn $ "User balance: " ++ show (balance $ addressState)
    putStrLn $ "tGasLimit ut: " ++ show (tGasLimit ut)
    putStrLn $ "blockGas: " ++ show blockGas
  
  (_, remainingBlockGas) <- addTransaction b blockGas t

  liftIO $ putStrLn $ "remainingBlockGas: " ++ show remainingBlockGas

  addTransactions b remainingBlockGas rest
  
addBlock::Block->ContextM ()
addBlock b@Block{blockData=bd, blockUncles=uncles} = do
  liftIO $ putStrLn $ "Attempting to insert block #" ++ show (number bd) ++ " (" ++ show (pretty $ blockHash b) ++ ")."
  maybeParent <- lift $ getBlock $ parentHash bd
  case maybeParent of
    Nothing ->
      liftIO $ putStrLn $ "Missing parent block in addBlock: " ++ show (pretty $ parentHash bd) ++ "\n" ++
      "Block will not be added now, but will be requested and added later"
    Just parentBlock -> do
      lift $ setStateRoot $ bStateRoot $ blockData parentBlock
      let rewardBase = 1500 * finney
      addToBalance (coinbase bd) rewardBase

      forM_ uncles $ \uncle -> do
                          addToBalance (coinbase bd) (rewardBase `quot` 32)
                          addToBalance (coinbase uncle) (rewardBase*15 `quot` 16)

      let transactions = receiptTransactions b
      addTransactions b (gasLimit $ blockData b) transactions

      dbs <- lift get
      liftIO $ putStrLn $ "newStateRoot: " ++ show (pretty $ stateRoot $ stateDB dbs)

      when (bStateRoot (blockData b) /= stateRoot (stateDB dbs)) $ do
        error $ "stateRoot mismatch!!  New stateRoot doesn't match block stateRoot: " ++ show (pretty $ bStateRoot $ blockData b)

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
  if number (blockData best) >= number (blockData b) 
       then return ()
       else lift $ detailsDBPut "best" (BL.toStrict $ encode $ blockHash b)

