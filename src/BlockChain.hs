{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module BlockChain (
  initializeBlockChain,
  addBlock,
  addBlocks,
  getBestBlock,
  getBestBlockHash
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Crypto.Hash.SHA3 as C
import Data.Binary
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Functor
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import qualified Database.LevelDB as DB

import Address
import AddressState
import Block
import Code
import Colors
import Constants
import DBs
import Environment
import Format
import ModifyStateDB
import RLP
import SHA
import SignedTransaction
import Transaction
import TransactionReceipt
import VM
import VMState

--import Debug.Trace

initializeBlockChain::DB->ResourceT IO ()
initializeBlockChain db = do
  let bytes = rlpSerialize $ rlpEncode genesisBlock
  DB.put (blockDB db) def (C.hash 256 bytes) bytes
  DB.put (detailsDB db) def "best" (BL.toStrict $ encode $ blockHash genesisBlock)

nextDifficulty::Integer->UTCTime->UTCTime->Integer
nextDifficulty oldDifficulty oldTime newTime =
    if (round (utcTimeToPOSIXSeconds newTime)) >=
           (round (utcTimeToPOSIXSeconds oldTime) + 42::Integer)
    then oldDifficulty - oldDifficulty `shiftR` 10
    else oldDifficulty + oldDifficulty `shiftR` 10

nextGasLimit::Integer->Integer->Integer
nextGasLimit oldGasLimit oldGasUsed = max 125000 ((oldGasLimit * 1023 + oldGasUsed *6 `quot` 5) `quot` 1024)

checkUnclesHash::Block->Bool
checkUnclesHash b = (unclesHash $ blockData b) == (hash $ rlpSerialize $ RLPArray (rlpEncode <$> blockUncles b))

--data BlockValidityError = BlockDifficultyWrong Integer Integer | BlockNumberWrong Integer Integer | BlockGasLimitWrong Integer Integer | BlockNonceWrong | BlockUnclesHashWrong
{-
instance Format BlockValidityError where
    --format BlockOK = "Block is valid"
    format (BlockDifficultyWrong d expected) = "Block difficulty is wrong, is '" ++ show d ++ "', expected '" ++ show expected ++ "'"
-}

verifyStateRootExists::DB->Block->ResourceT IO Bool
verifyStateRootExists db b = do
  val <- DB.get (stateDB db) def (BL.toStrict $ encode $ stateRoot $ blockData b)
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

checkValidity::Monad m=>DB->Block->ResourceT IO (m ())
checkValidity db b = do
  maybeParentBlock <- getBlock db (parentHash $ blockData b)
  case maybeParentBlock of
    Just parentBlock -> do
          checkParentChildValidity b parentBlock
          unless (nonceIsValid b) $ fail $ "Block nonce is wrong: " ++ format b
          unless (checkUnclesHash b) $ fail "Block unclesHash is wrong"
          stateRootExists <- verifyStateRootExists db b
          unless stateRootExists $ fail ("Block stateRoot does not exist: " ++ format (stateRoot $ blockData b))
          return $ return ()
    Nothing -> fail ("Parent Block does not exist: " ++ format (parentHash $ blockData b))


{-
                    coinbase=prvKey2Address prvKey,
        stateRoot = SHA 0x9b109189563315bfeb13d4bfd841b129ff3fd5c85f228a8d9d8563b4dde8432e,
                    transactionsTrie = 0,
-}


chargeForCodeRun::DB->SHAPtr->Address->Address->Integer->ResourceT IO SHAPtr
chargeForCodeRun db p a theCoinbase val = do
  p2 <- addToBalance db p a (-val)
  addToBalance db p2 theCoinbase val

runCodeForTransaction::DB->SHAPtr->Block->SignedTransaction->ResourceT IO SHAPtr
runCodeForTransaction _ p _ SignedTransaction{unsignedTransaction=Transaction{tInit=Code c}} | B.null c = return p
runCodeForTransaction db p b t@SignedTransaction{unsignedTransaction=ut} = do
  vmState <-
    liftIO $ runCodeFromStart db p (tGasLimit $ unsignedTransaction t)
    Environment{
      envGasPrice=gasPrice ut,
      envBlock=b,
      envOwner = whoSignedThisTransaction t,
      envOrigin = undefined,
      envInputData = undefined,
      envSender = undefined,
      envValue = undefined,
      envCode = tInit ut
      }
  result <- liftIO $ getReturnValue vmState
  liftIO $ putStrLn $ "Result: " ++ show result
  case result of
    Left err -> do
      liftIO $ putStrLn $ red $ show err
      return p
    Right resultBytes -> do
      let newAddress = getNewAddress t
      liftIO $ putStrLn $ format newAddress ++ ": " ++ format resultBytes
      --TODO- I think there is an error in the cpp ethereum, no new account it made
      --when value doesn't equal 0....  I am mimicking this here so that I can work with that
      --client, but I really should either try to understand this better or if I convince myself
      --that there is a bug, report it.
      p2 <- if value ut == 0
            then addNewAccount db p newAddress resultBytes
            else return p
      liftIO $ putStrLn $ "gasRemaining: " ++ show (vmGasRemaining vmState)
      let usedGas = tGasLimit ut - vmGasRemaining vmState
      liftIO $ putStrLn $ "gasUsed: " ++ show usedGas
      chargeForCodeRun db p2 (whoSignedThisTransaction t) (coinbase $ blockData b) (usedGas * gasPrice ut)

addBlocks::DB->[Block]->ResourceT IO ()
addBlocks db blocks = do
  forM_ blocks $ addBlock db

getNewAddress::SignedTransaction->Address
getNewAddress t =
  let theHash = hash $ rlpSerialize $ RLPArray [rlpEncode $ whoSignedThisTransaction t, rlpEncode $ tNonce $ unsignedTransaction t]
  in decode $ BL.drop 12 $ encode $ theHash

isTransactionValid::DB->SHAPtr->SignedTransaction->ResourceT IO Bool
isTransactionValid db p t = do
  maybeAddressState <- getAddressState db p $ whoSignedThisTransaction t
  case maybeAddressState of
    Nothing -> return (0 == tNonce (unsignedTransaction t))
    Just addressState -> return (addressStateNonce addressState == tNonce (unsignedTransaction t))

addTransaction::DB->SHAPtr->Block->SignedTransaction->ResourceT IO SHAPtr
addTransaction db sr b t = do
  liftIO $ putStrLn "adding to nonces"
  let signAddress = whoSignedThisTransaction t
  sr2 <- addNonce db sr signAddress
  sr3 <- chargeFees db sr2 (coinbase $ blockData b) t
  sr4 <- chargeForCodeSize db sr3 (coinbase $ blockData b) t
  sr5 <- if to (unsignedTransaction t) == Address 0
         then addToBalance db sr4 signAddress (-value (unsignedTransaction t))
         else transferEther db sr4 signAddress (to $ unsignedTransaction t) (value (unsignedTransaction t))
  runCodeForTransaction db sr5 b t

addTransactions::DB->SHAPtr->Block->[SignedTransaction]->ResourceT IO SHAPtr
addTransactions _ sr _ [] = return sr
addTransactions db sr b (t:rest) = do
  valid <- isTransactionValid db sr t
  sr2 <- if valid
         then addTransaction db sr b t
         else return sr
  addTransactions db sr2 b rest
  

addBlock::DB->Block->ResourceT IO ()
addBlock db b = do
  let bd = blockData b
  parentBlock <-
    fromMaybe (error ("Missing parent block in addBlock: " ++ format (parentHash bd))) <$>
    (getBlock db $ parentHash bd)

  sr2 <- addToBalance db (stateRoot $ blockData parentBlock) (coinbase bd) (1500*finney)

  let transactions = theTransaction <$> receiptTransactions b

  sr3 <- addTransactions db sr2 b transactions
  

  liftIO $ putStrLn $ "newStateRoot: " ++ format sr3

  valid <- checkValidity db b
  case valid of
     Right () -> return ()
     Left err -> error err
  let bytes = rlpSerialize $ rlpEncode b
  DB.put (blockDB db) def (C.hash 256 bytes) bytes
  replaceBestIfBetter db b


chargeFees::DB->SHAPtr->Address->SignedTransaction->ResourceT IO SHAPtr
chargeFees db sr theCoinbase t = do
  sr2 <- addToBalance db sr theCoinbase (5*finney)
  addToBalance db sr2 (whoSignedThisTransaction t) (-5*finney)
  
chargeForCodeSize::DB->SHAPtr->Address->SignedTransaction->ResourceT IO SHAPtr
chargeForCodeSize db sr theCoinbase t = do
  let codeSize = codeLength $ tInit $ unsignedTransaction t
  let val = 5 * (gasPrice $ unsignedTransaction t) * fromIntegral codeSize
  sr2 <- addToBalance db sr theCoinbase val
  addToBalance db sr2 (whoSignedThisTransaction t) (-val)
  

getBestBlockHash::DB->ResourceT IO (Maybe SHA)
getBestBlockHash db = do
  fmap (decode . BL.fromStrict) <$> DB.get (detailsDB db) def "best"

getBlock::DB->SHA->ResourceT IO (Maybe Block)
getBlock db h = 
  fmap (rlpDecode . rlpDeserialize) <$> DB.get (blockDB db) def (BL.toStrict $ encode h)

getBestBlock::DB->ResourceT IO (Maybe Block)
getBestBlock db = do
  maybeH <- getBestBlockHash db
  case maybeH of
    Nothing -> return Nothing
    Just h -> getBlock db h

replaceBestIfBetter::DB->Block->ResourceT IO ()
replaceBestIfBetter db b = do
  maybeBest <- getBestBlock db
  case maybeBest of
    Just best | number (blockData best) >= number (blockData b) -> return ()
    _ -> runResourceT $ do
      DB.put (detailsDB db) def "best" (BL.toStrict $ encode $ blockHash b)

