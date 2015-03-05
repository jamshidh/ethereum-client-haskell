
module Blockchain.Context (
  Context(..),
  ContextM,
  initContext
  ) where


import qualified Database.LevelDB as DB

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import System.Directory
import System.FilePath
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Constants
import Blockchain.DBM
import Blockchain.Database.MerklePatricia
import Blockchain.Data.Peer
import Blockchain.SHA

--import Debug.Trace

type BlockDB = DB.DB
type CodeDB = DB.DB
type DetailsDB = DB.DB
type StorageDB = MPDB

data Context =
  Context {
    neededBlockHashes::[SHA],
    pingCount::Int,
    peers::[Peer]
    }

type ContextM = StateT Context DBM

options::DB.Options
options = DB.defaultOptions {
  DB.createIfMissing=True, DB.cacheSize=1024}

initContext::String->ResourceT IO Context
initContext theType = do
  homeDir <- liftIO getHomeDirectory                     
  liftIO $ createDirectoryIfMissing False $ homeDir </> dbDir theType
  bdb <- DB.open (homeDir </> dbDir theType ++ blockDBPath) options
  ddb <- DB.open (homeDir </> dbDir theType ++ detailsDBPath) options
  sdb <- DB.open (homeDir </> dbDir theType ++ stateDBPath) options
  return $ Context
      []
      0
      []
