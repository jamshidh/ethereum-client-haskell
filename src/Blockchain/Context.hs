{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Blockchain.Context (
  Context(..),
  ContextM,
  getDebugMsg,
  addDebugMsg,
  clearDebugMsg
  ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.State
import qualified Data.ByteString as B
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.DBM
import Blockchain.Data.Peer
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import qualified Blockchain.Database.MerklePatricia as MPDB
import Blockchain.DB.HashDB
import Blockchain.DB.SQLDB
import Blockchain.DB.StateDB
import Blockchain.DB.StorageDB
import Blockchain.Options
import Blockchain.SHA

--import Debug.Trace

data Context =
  Context {
    contextHashDB::HashDB,
    contextSQLDB::SQLDB,
    neededBlockHashes::[SHA],
    pingCount::Int,
    peers::[Peer],
    miningDataset::B.ByteString,
    vmTrace::[String]
    }

type ContextM = StateT Context (ResourceT IO)

instance HasHashDB ContextM where
  getHashDB = fmap contextHashDB get

instance HasSQLDB ContextM where
  getSQLDB = fmap contextSQLDB get

{-
initContext::String->IO Context
initContext theType = do
  liftIO $ putStr "Loading mining cache.... "
  hFlush stdout
  dataset <- return "" -- mmapFileByteString "dataset0" Nothing
  liftIO $ putStrLn "Finished"
  homeDir <- getHomeDirectory                     
  createDirectoryIfMissing False $ homeDir </> dbDir theType
  return $ Context
      []
      0
      []
      dataset
      False
-}

getDebugMsg::ContextM String
getDebugMsg = do
  cxt <- get
  return $ concat $ reverse $ vmTrace cxt

addDebugMsg::String->ContextM ()
addDebugMsg msg = do
  cxt <- get
  put cxt{vmTrace=msg:vmTrace cxt}

clearDebugMsg::ContextM ()
clearDebugMsg = do
  cxt <- get
  put cxt{vmTrace=[]}
