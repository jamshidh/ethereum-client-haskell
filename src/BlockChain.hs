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

import Block
import RLP
import SHA

import Debug.Trace

type BlockDB = DB.DB
type DetailsDB = DB.DB

blockDBPath::String
blockDBPath="/home/jim/.ethereumH/blocks/"
detailsDBPath::String
detailsDBPath="/home/jim/.ethereumH/details/"

options::DB.Options
options = DB.defaultOptions {
  DB.createIfMissing=True, DB.cacheSize=1024}
          
addBlocks::[Block]->IO ()
addBlocks blocks = runResourceT $ do
  bdb <- DB.open blockDBPath options
  ddb <- DB.open detailsDBPath options
  forM_ blocks (addBlock bdb ddb)

addBlock::BlockDB->DetailsDB->Block->ResourceT IO ()
addBlock bdb ddb b = trace ("addBlock: " ++ show (number $ blockData b)) $ do
  --TODO- check for block validity, throw away if bad
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
replaceBestIfBetter bdb ddb b = trace "qqqqqqqqqqqqqqqqqqreplaceBestIfBetter" $ do
  maybeBest <- getBestBlock bdb ddb
  liftIO $ print $ maybeBest
  case maybeBest of
    Just best | number (blockData best) >= number (blockData b) -> return ()
    Nothing -> runResourceT $ do
      DB.put ddb def "best" (BL.toStrict $ encode $ blockHash b)
    _ -> return ()

--withBlockDB::(BlockDB->a)->a
withBlockDB f = do
  runResourceT $ do
    bdb  <- DB.open detailsDBPath options
    f bdb
