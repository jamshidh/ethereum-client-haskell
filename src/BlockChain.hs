{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module BlockChain (
  initializeBlockChain,
  addBlock,
  addBlocks,
  getBestBlock,
  getBestBlockHash,
  withBlockDB
  ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Crypto.Hash.SHA3 as C
import Data.Binary
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Functor
import Data.Time
import Data.Time.Clock.POSIX
import qualified Database.LevelDB as DB
import System.Directory

import Block
import Format
import RLP
import SHA

--import Debug.Trace

type BlockDB = DB.DB
type DetailsDB = DB.DB

blockDBPath::String
blockDBPath="/.ethereumH/blocks/"
detailsDBPath::String
detailsDBPath="/.ethereumH/details/"
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

checkParentChildValidity::(Monad m)=>Block->Block->m ()
checkParentChildValidity Block{blockData=c} Block{blockData=p} = do
    unless (difficulty c == nextDifficulty (difficulty p) (timestamp p) ( timestamp c))
             $ fail $ "Block difficulty is wrong: got '" ++ show (difficulty c) ++ "', expected '" ++ show (nextDifficulty (difficulty p) (timestamp p) ( timestamp c)) ++ "'"
    unless (number c == number p + 1) 
             $ fail $ "Block number is wrong: got '" ++ show (number c) ++ ", expected '" ++ show (number p + 1) ++ "'"
    unless (gasLimit c == nextGasLimit (gasLimit p) (gasUsed p))
             $ fail $ "Block gasLimit is wrong: got '" ++ show (gasLimit c) ++ "', expected '" ++ show (nextGasLimit (gasLimit p) (gasUsed p)) ++ "'"
    return ()

checkValidity::Monad m=>BlockDB->Block->ResourceT IO (m ())
checkValidity bdb b = do
  maybeParentBlock <- getBlock bdb (parentHash $ blockData b)
  case maybeParentBlock of
    Just parentBlock -> do
          checkParentChildValidity b parentBlock
          unless (nonceIsValid b) $ fail "Block nonce is wrong"
          unless (checkUnclesHash b) $ fail "Block unclesHash is wrong"
          return $ return ()
    _ -> fail ("Parent Block does not exist" ++ format (parentHash $ blockData b))


{-
        return undefined -- $ return ()
-}
{-
                    coinbase=prvKey2Address prvKey,
        stateRoot = SHA 0x9b109189563315bfeb13d4bfd841b129ff3fd5c85f228a8d9d8563b4dde8432e,
                    transactionsTrie = 0,
-}




 

addBlocks::[Block]->IO ()
addBlocks blocks = runResourceT $ do
  homeDir <- liftIO $ getHomeDirectory                     
  bdb <- DB.open (homeDir ++ blockDBPath) options
  ddb <- DB.open (homeDir ++ detailsDBPath) options
  forM_ blocks (addBlock bdb ddb)

addBlock::BlockDB->DetailsDB->Block->ResourceT IO ()
addBlock bdb ddb b = do
  --TODO- check for block validity, throw away if bad
  valid <- checkValidity bdb b
  case valid of
     Right () -> return ()
     Left err -> error err
  let bytes = rlpSerialize $ rlpEncode b
  DB.put bdb def (C.hash 256 bytes) bytes
  replaceBestIfBetter bdb ddb b

getBestBlockHash::DetailsDB->ResourceT IO (Maybe SHA)
getBestBlockHash ddb = do
  fmap (decode . BL.fromStrict) <$> DB.get ddb def "best"

getBlock::BlockDB->SHA->ResourceT IO (Maybe Block)
getBlock bdb h = runResourceT $ do
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

--withBlockDB::(BlockDB->a)->a
withBlockDB f = do
  runResourceT $ do
    homeDir <- liftIO $ getHomeDirectory
    bdb  <- DB.open (homeDir ++ detailsDBPath) options
    f bdb
