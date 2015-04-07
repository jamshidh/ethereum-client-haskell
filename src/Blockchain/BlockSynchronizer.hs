
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
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.BlockChain
import Blockchain.Communication
import Blockchain.Context
import Blockchain.Data.DataDefs
import Blockchain.ExtDBs
import Blockchain.Data.Wire
import Blockchain.Frame

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

handleNewBlockHashes::[SHA]->EthCryptM ContextM ()
--handleNewBlockHashes _ list | trace ("########### handleNewBlockHashes: " ++ show list) $ False = undefined
handleNewBlockHashes [] = error "handleNewBlockHashes called with empty list"
handleNewBlockHashes blockHashes = do
  result <- lift $ findFirstHashAlreadyInDB blockHashes
  case result of
    Nothing -> do
                --liftIO $ putStrLn "Requesting more block hashes"
                cxt <- lift get 
                lift $ put cxt{neededBlockHashes=reverse blockHashes ++ neededBlockHashes cxt}
                sendMsg $ GetBlockHashes [last blockHashes] 0x500
    Just hashInDB -> do
                liftIO $ putStrLn $ "Found a serverblock already in our database: " ++ show (pretty hashInDB)
                cxt <- lift get
                --liftIO $ putStrLn $ show (pretty blockHashes)
                lift $ put cxt{neededBlockHashes=reverse (takeWhile (/= hashInDB) blockHashes) ++ neededBlockHashes cxt}
                askForSomeBlocks
  
askForSomeBlocks::EthCryptM ContextM ()
askForSomeBlocks = do
  cxt <- lift get
  if null (neededBlockHashes cxt)
    then return ()
    else do
      let (firstBlocks, lastBlocks) = splitAt 0x20 (neededBlockHashes cxt)
      lift $ put cxt{neededBlockHashes=lastBlocks}
      sendMsg $ GetBlocks firstBlocks


handleNewBlocks::[Block]->EthCryptM ContextM ()
handleNewBlocks blocks = do
  liftIO $ putStrLn "Submitting new blocks"
  lift $ addBlocks $ sortBy (compare `on` blockDataNumber . blockBlockData) blocks
  liftIO $ putStrLn $ show (length blocks) ++ " blocks have been submitted"
  askForSomeBlocks
