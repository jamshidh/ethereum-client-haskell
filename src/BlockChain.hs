{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module BlockChain (
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
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Functor
import qualified Database.LevelDB as DB
import System.Directory

import Block
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

checkValidity::BlockDB->Block->ResourceT IO Bool
checkValidity bdb b = do
  maybeParentBlock <- getBlock bdb (parentHash $ blockData b)
  case maybeParentBlock of
    Just parentBlock -> return True
    _ -> return False
 

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
  if valid
     then return ()
     else error $ "error in addBlock: block parent does not exist"
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
  liftIO $ print $ maybeBest
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
