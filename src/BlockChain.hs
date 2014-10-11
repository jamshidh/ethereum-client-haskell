{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module BlockChain (
  addBlock,
  getBestBlock,
  getBestBlockHash
) where

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

blockDBPath::String
blockDBPath="/home/jim/.ethereumH/blocks/"
detailsDBPath::String
detailsDBPath="/home/jim/.ethereumH/details/"

options::DB.Options
options = DB.defaultOptions {
  DB.createIfMissing=True, DB.cacheSize=1024}
          
addBlock::Block->IO ()
addBlock b = trace ("addBlock: " ++ show (number $ blockData b)) $ do
  --TODO- check for block validity, throw away if bad
  runResourceT $ do
    db <- DB.open blockDBPath options
    let bytes = rlpSerialize $ rlpEncode b
    DB.put db def (C.hash 256 bytes) bytes

  replaceBestIfBetter b

getBestBlockHash::IO (Maybe SHA)
getBestBlockHash = runResourceT $ do
  db <- DB.open detailsDBPath options
  fmap (decode . BL.fromStrict) <$> DB.get db def "best"

getBlock::SHA->IO (Maybe Block)
getBlock h = runResourceT $ do
  db <- DB.open blockDBPath options
  fmap (rlpDecode . rlpDeserialize) <$> DB.get db def (BL.toStrict $ encode h)

getBestBlock::IO (Maybe Block)
getBestBlock = do
  maybeH <- getBestBlockHash
  case maybeH of
    Nothing -> return Nothing
    Just h -> getBlock h

replaceBestIfBetter::Block->IO ()
replaceBestIfBetter b = do
  maybeBest <- getBestBlock
  case maybeBest of
    Just best | number (blockData best) >= number (blockData b) -> return ()
    _ -> runResourceT $ do
      db <- DB.open detailsDBPath options
      DB.put db def "best" (BL.toStrict $ encode $ blockHash b)
