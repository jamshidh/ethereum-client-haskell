
module Blockchain.BlockSynchronizer (
                          handleNewBlockHashes,
                          handleNewBlocks
                         ) where

import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as BL
import Data.Function
import Data.List
import Data.Maybe
import System.IO
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.Block
import Blockchain.BlockChain
import Blockchain.Communication
import Blockchain.Context
import Blockchain.ExtDBs
import Blockchain.SHA
import Blockchain.Data.Wire

--import Debug.Trace

data GetBlockHashesResult = NeedMore SHA | NeededHashes [SHA] deriving (Show)

findFirstHashAlreadyInDB::[SHA]->ContextM (Maybe SHA)
findFirstHashAlreadyInDB hashes = do
  items <- lift $ filterM (fmap (not . isNothing) . blockDBGet . BL.toStrict . Bin.encode) hashes
  return $ safeHead items
  where
    safeHead::[a]->Maybe a
    safeHead [] = Nothing
    safeHead (x:_) = Just x

handleNewBlockHashes::Handle->[SHA]->ContextM ()
--handleNewBlockHashes _ list | trace ("########### handleNewBlockHashes: " ++ show list) $ False = undefined
handleNewBlockHashes _ [] = error "handleNewBlockHashes called with empty list"
handleNewBlockHashes handle blockHashes = do
  result <- findFirstHashAlreadyInDB blockHashes
  case result of
    Nothing -> do
                --liftIO $ putStrLn "Requesting more block hashes"
                cxt <- get 
                put cxt{neededBlockHashes=reverse blockHashes ++ neededBlockHashes cxt}
                sendMessage handle $ GetBlockHashes [last blockHashes] 0x500
    Just hashInDB -> do
                liftIO $ putStrLn $ "Found a serverblock already in our database: " ++ show (pretty hashInDB)
                cxt <- get
                --liftIO $ putStrLn $ show (pretty blockHashes)
                put cxt{neededBlockHashes=reverse (takeWhile (/= hashInDB) blockHashes) ++ neededBlockHashes cxt}
                askForSomeBlocks handle
  
askForSomeBlocks::Handle->ContextM ()
askForSomeBlocks handle = do
  cxt <- get
  if null (neededBlockHashes cxt)
    then return ()
    else do
      let (firstBlocks, lastBlocks) = splitAt 0x20 (neededBlockHashes cxt)
      put cxt{neededBlockHashes=lastBlocks}
      sendMessage handle $ GetBlocks firstBlocks


handleNewBlocks::Handle->[Block]->ContextM ()
handleNewBlocks handle blocks = do
  liftIO $ putStrLn "Submitting new blocks"
  addBlocks $ sortBy (compare `on` number . blockData) blocks
  liftIO $ putStrLn $ show (length blocks) ++ " blocks have been submitted"
  askForSomeBlocks handle
