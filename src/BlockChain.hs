{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module BlockChain (
  initializeBlockChain,
  addBlock,
  addBlocks,
  getBestBlock,
  getBestBlockHash,
  withBlockDB
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
import System.Directory

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
import Transaction
import TransactionReceipt
import VM
import VMState

--import Debug.Trace

options::DB.Options
options = DB.defaultOptions {
  DB.createIfMissing=True, DB.cacheSize=1024}

initializeBlockChain::IO ()
initializeBlockChain = do
  homeDir <- liftIO $ getHomeDirectory                     
  runResourceT $ do
    bdb <- DB.open (homeDir ++ blockDBPath) options
    ddb <- DB.open (homeDir ++ detailsDBPath) options
    let bytes = rlpSerialize $ rlpEncode genesisBlock
    DB.put bdb def (C.hash 256 bytes) bytes
    DB.put ddb def "best" (BL.toStrict $ encode $ blockHash genesisBlock)

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

verifyStateRootExists::StateDB->Block->ResourceT IO Bool
verifyStateRootExists sdb b = do
  val <- DB.get sdb def (BL.toStrict $ encode $ stateRoot $ blockData b)
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

checkValidity::Monad m=>BlockDB->StateDB->Block->ResourceT IO (m ())
checkValidity bdb sdb b = do
  maybeParentBlock <- getBlock bdb (parentHash $ blockData b)
  case maybeParentBlock of
    Just parentBlock -> do
          checkParentChildValidity b parentBlock
          unless (nonceIsValid b) $ fail $ "Block nonce is wrong: " ++ format b
          unless (checkUnclesHash b) $ fail "Block unclesHash is wrong"
          stateRootExists <- verifyStateRootExists sdb b
          unless stateRootExists $ fail ("Block stateRoot does not exist: " ++ format (stateRoot $ blockData b))
          return $ return ()
    Nothing -> fail ("Parent Block does not exist: " ++ format (parentHash $ blockData b))


{-
                    coinbase=prvKey2Address prvKey,
        stateRoot = SHA 0x9b109189563315bfeb13d4bfd841b129ff3fd5c85f228a8d9d8563b4dde8432e,
                    transactionsTrie = 0,
-}


chargeForCodeRun::StateDB->SHAPtr->Address->Address->Integer->ResourceT IO SHAPtr
chargeForCodeRun sdb p a theCoinbase val = do
  p2 <- addToBalance sdb p a (-val)
  addToBalance sdb p2 theCoinbase val

runCodeForTransaction::StateDB->SHAPtr->Block->Transaction->ResourceT IO SHAPtr
runCodeForTransaction _ p _ Transaction{tInit=Code c} | B.null c = return p
runCodeForTransaction sdb p b t = do
  vmState <-
    liftIO $ runCodeFromStart sdb p
    Environment{
      envGasPrice=gasPrice t,
      envBlock=b,
      envOwner = whoSignedThisTransaction t,
      envOrigin = undefined,
      envInputData = undefined,
      envSender = undefined,
      envValue = undefined,
      envCode = tInit t
      }
  result <- liftIO $ getReturnValue vmState
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
      p2 <- if value t == 0
            then addNewAccount sdb p newAddress resultBytes
            else return p
      liftIO $ putStrLn $ "gasRemaining: " ++ show (vmGasRemaining vmState)
      let usedGas = tGasLimit t - vmGasRemaining vmState
      liftIO $ putStrLn $ "gasUsed: " ++ show usedGas
      chargeForCodeRun sdb p2 (whoSignedThisTransaction t) (coinbase $ blockData b) (usedGas * gasPrice t)

runAllCode::StateDB->SHAPtr->Block->Transaction->ResourceT IO SHAPtr
runAllCode sdb p b t = runCodeForTransaction sdb p b t
 

addBlocks::[Block]->IO ()
addBlocks blocks = runResourceT $ do
  homeDir <- liftIO $ getHomeDirectory                     
  bdb <- DB.open (homeDir ++ blockDBPath) options
  ddb <- DB.open (homeDir ++ detailsDBPath) options
  sdb <- DB.open (homeDir ++ stateDBPath) options
  forM_ blocks (addBlock bdb ddb sdb)

getNewAddress::Transaction->Address
getNewAddress t =
  let theHash = hash $ rlpSerialize $ RLPArray [rlpEncode $ whoSignedThisTransaction t, rlpEncode $ tNonce t]
  in decode $ BL.drop 12 $ encode $ theHash

isTransactionValid::StateDB->SHAPtr->Transaction->ResourceT IO Bool
isTransactionValid sdb p t = do
  maybeAddressState <- getAddressState sdb p $ whoSignedThisTransaction t
  case maybeAddressState of
    Nothing -> return (0 == tNonce t)
    Just addressState -> return (addressStateNonce addressState == tNonce t)

addTransaction::StateDB->SHAPtr->Block->Transaction->ResourceT IO SHAPtr
addTransaction sdb sr b t = do
  liftIO $ putStrLn "adding to nonces"
  let signAddress = whoSignedThisTransaction t
  sr2 <- addNonce sdb sr signAddress
  sr3 <- chargeFees sdb sr2 (coinbase $ blockData b) t
  sr4 <- chargeForCodeSize sdb sr3 (coinbase $ blockData b) t
  sr5 <- if to t == Address 0
         then addToBalance sdb sr4 signAddress (-value t)
         else transferEther sdb sr4 signAddress (to t) (value t)
  runAllCode sdb sr5 b t

addTransactions::StateDB->SHAPtr->Block->[Transaction]->ResourceT IO SHAPtr
addTransactions _ sr _ [] = return sr
addTransactions sdb sr b (t:rest) = do
  valid <- isTransactionValid sdb sr t
  sr2 <- if valid
         then addTransaction sdb sr b t
         else return sr
  addTransactions sdb sr2 b rest
  

addBlock::BlockDB->DetailsDB->StateDB->Block->ResourceT IO ()
addBlock bdb ddb sdb b = do
  let bd = blockData b
  parentBlock <-
    fromMaybe (error ("Missing parent block in addBlock: " ++ format (parentHash bd))) <$>
    (getBlock bdb $ parentHash bd)

  sr2 <- addToBalance sdb (stateRoot $ blockData parentBlock) (coinbase bd) (1500*finney)

  let transactions = theTransaction <$> receiptTransactions b

  sr3 <- addTransactions sdb sr2 b transactions
  

  liftIO $ putStrLn $ "newStateRoot: " ++ format sr3

  valid <- checkValidity bdb sdb b
  case valid of
     Right () -> return ()
     Left err -> error err
  let bytes = rlpSerialize $ rlpEncode b
  DB.put bdb def (C.hash 256 bytes) bytes
  replaceBestIfBetter bdb ddb b


chargeFees::StateDB->SHAPtr->Address->Transaction->ResourceT IO SHAPtr
chargeFees sdb sr theCoinbase t = do
  sr2 <- addToBalance sdb sr theCoinbase (5*finney)
  addToBalance sdb sr2 (whoSignedThisTransaction t) (-5*finney)
  
chargeForCodeSize::StateDB->SHAPtr->Address->Transaction->ResourceT IO SHAPtr
chargeForCodeSize sdb sr theCoinbase t = do
  let codeSize = codeLength $ tInit t
  let val = 5 * (gasPrice t) * fromIntegral codeSize
  sr2 <- addToBalance sdb sr theCoinbase val
  addToBalance sdb sr2 (whoSignedThisTransaction t) (-val)
  

getBestBlockHash::DetailsDB->ResourceT IO (Maybe SHA)
getBestBlockHash ddb = do
  fmap (decode . BL.fromStrict) <$> DB.get ddb def "best"

getBlock::BlockDB->SHA->ResourceT IO (Maybe Block)
getBlock bdb h = 
  fmap (rlpDecode . rlpDeserialize) <$> DB.get bdb def (BL.toStrict $ encode h)

getBestBlock::BlockDB->DetailsDB->ResourceT IO (Maybe Block)
getBestBlock bdb ddb = do
  maybeH <- getBestBlockHash ddb
  case maybeH of
    Nothing -> return Nothing
    Just h -> getBlock bdb h

replaceBestIfBetter::BlockDB->DetailsDB->Block->ResourceT IO ()
replaceBestIfBetter bdb ddb b = do
  maybeBest <- getBestBlock bdb ddb
  case maybeBest of
    Just best | number (blockData best) >= number (blockData b) -> return ()
    _ -> runResourceT $ do
      DB.put ddb def "best" (BL.toStrict $ encode $ blockHash b)

withBlockDB::(MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
     (DB.DB -> ResourceT m a)-> m a
withBlockDB f = do
  runResourceT $ do
    homeDir <- liftIO $ getHomeDirectory
    bdb  <- DB.open (homeDir ++ detailsDBPath) options
    f bdb
