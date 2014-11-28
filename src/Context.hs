
module Context (
  Context(..),
  ContextM,
  setStateRoot,
  setStorageStateRoot,
  openDBs,
  DetailsDB,
  BlockDB
  ) where


import qualified Database.LevelDB as DB

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import System.Directory

import Constants
import Database.MerklePatricia

type BlockDB = DB.DB
type CodeDB = DB.DB
type DetailsDB = DB.DB
type StorageDB = MPDB

data Context =
  Context {
    blockDB::BlockDB,
    detailsDB::DetailsDB,
    stateDB::MPDB,
    codeDB::CodeDB,
    storageDB::StorageDB
    }

type ContextM = StateT Context IO

setStateRoot::SHAPtr->ContextM ()
setStateRoot stateRoot' = do
  ctx <- get
  put ctx{stateDB=(stateDB ctx){stateRoot=stateRoot'}}

setStorageStateRoot::SHAPtr->ContextM ()
setStorageStateRoot stateRoot' = do
  ctx <- get
  put ctx{storageDB=(storageDB ctx){stateRoot=stateRoot'}}

options::DB.Options
options = DB.defaultOptions {
  DB.createIfMissing=True, DB.cacheSize=1024}

openDBs::Bool->ResourceT IO Context
openDBs useCppDBs = do
  homeDir <- liftIO getHomeDirectory                     
  bdb <- DB.open (homeDir ++ dbDir useCppDBs ++ blockDBPath) options
  ddb <- DB.open (homeDir ++ dbDir useCppDBs ++ detailsDBPath) options
  sdb <- DB.open (homeDir ++ dbDir useCppDBs ++ stateDBPath) options
  return $ Context
      bdb
      ddb
      MPDB{ ldb=sdb, stateRoot=error "no stateRoot defined"}
      sdb
      MPDB{ ldb=sdb, stateRoot=error "no stateRoot defined"}
