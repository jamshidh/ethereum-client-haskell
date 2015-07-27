{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Blockchain.Context (
  Context(..),
  ContextM,
  getDebugMsg,
  addDebugMsg,
  clearDebugMsg,
  incrementNonce,
  getNewAddress
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
import Blockchain.DB.CodeDB
import Blockchain.DB.HashDB
import Blockchain.Options
import Blockchain.SHA

--import Debug.Trace

data Context =
  Context {
    contextStateDB::MPDB.MPDB,
    contextHashDB::HashDB,
    contextBlockDB::BlockDB,
    contextCodeDB::CodeDB,
    contextSQLDB::SQLDB,
    contextDetailsDB::DetailsDB,
    vmTrace::[String]
    }

type ContextM = StateT Context (ResourceT IO)

instance HasStateDB ContextM where
  getStateDB = do
    cxt <- get
    return $ contextStateDB cxt
  setStateDBStateRoot sr = do
    cxt <- get
    put cxt{contextStateDB=(contextStateDB cxt){MPDB.stateRoot=sr}}

instance HasStorageDB ContextM where
  getStorageDB = do
    cxt <- get
    return $ MPDB.ldb $ contextStateDB cxt --storage and states use the same database!

instance HasHashDB ContextM where
  getHashDB = fmap contextHashDB get

instance HasBlockDB ContextM where
  getBlockDB = fmap contextBlockDB get

instance HasCodeDB ContextM where
  getCodeDB = fmap contextCodeDB get

instance HasSQLDB ContextM where
  getSQLDB = fmap contextSQLDB get

instance HasDetailsDB ContextM where
  getDetailsDB = fmap contextDetailsDB get

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

incrementNonce::Address->ContextM ()
incrementNonce address = do
  addressState <- getAddressState address
  putAddressState address addressState{ addressStateNonce = addressStateNonce addressState + 1 }

getNewAddress::Address->ContextM Address
getNewAddress address = do
  addressState <- getAddressState address
  when flags_debug $ liftIO $ putStrLn $ "Creating new account: owner=" ++ show (pretty address) ++ ", nonce=" ++ show (addressStateNonce addressState)
  let newAddress = getNewAddress_unsafe address (addressStateNonce addressState)
  incrementNonce address
  return newAddress











