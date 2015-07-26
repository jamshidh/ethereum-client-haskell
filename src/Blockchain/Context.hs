{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Blockchain.Context (
  Context(..),
  ContextM,
  getStorageKeyVal',
  getAllStorageKeyVals',
  getDebugMsg,
  addDebugMsg,
  clearDebugMsg,
  putStorageKeyVal',
  deleteStorageKey',
  incrementNonce,
  getNewAddress
  ) where


import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.NibbleString as N
import qualified Database.LevelDB as LDB
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.DBM
import Blockchain.Data.Peer
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.RLP
import qualified Blockchain.Database.MerklePatricia as MPDB
import qualified Blockchain.Database.MerklePatricia.Internal as MPDB
import Blockchain.DB.CodeDB
import Blockchain.DB.HashDB
import Blockchain.ExtDBs
import Blockchain.ExtWord
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
    neededBlockHashes::[SHA],
    pingCount::Int,
    peers::[Peer],
    miningDataset::B.ByteString,
    vmTrace::[String]
    }

type ContextM = StateT Context DBM

instance HasStateDB ContextM where
  getStateDB = do
    cxt <- get
    return $ contextStateDB cxt
  setStateDBStateRoot sr = do
    cxt <- get
    put cxt{contextStateDB=(contextStateDB cxt){MPDB.stateRoot=sr}}

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

getStorageKeyVal'::Address->Word256->ContextM Word256
getStorageKeyVal' owner key = do
  addressState <- getAddressState owner
  db <- getStateDB
  let mpdb = db{MPDB.stateRoot=addressStateContractRoot addressState}
  maybeVal <- lift $ lift $ MPDB.getKeyVal mpdb (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key)
  case maybeVal of
    Nothing -> return 0
    Just x -> return $ fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode x

getAllStorageKeyVals'::Address->ContextM [(MPDB.Key, Word256)]
getAllStorageKeyVals' owner = do
  addressState <- getAddressState owner
  db <- getStateDB
  let mpdb = db{MPDB.stateRoot=addressStateContractRoot addressState}
  kvs <- lift $ lift $ MPDB.unsafeGetAllKeyVals mpdb
  return $ map (fmap $ fromInteger . rlpDecode . rlpDeserialize . rlpDecode) kvs

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

putStorageKeyVal'::Address->Word256->Word256->ContextM ()
putStorageKeyVal' owner key val = do
  hashDBPut storageKeyNibbles
  addressState <- getAddressState owner
  db <- getStateDB
  let mpdb = db{MPDB.stateRoot=addressStateContractRoot addressState}
  newContractRoot <- fmap MPDB.stateRoot $ lift $ lift $ MPDB.putKeyVal mpdb storageKeyNibbles (rlpEncode $ rlpSerialize $ rlpEncode $ toInteger val)
  putAddressState owner addressState{addressStateContractRoot=newContractRoot}
  where storageKeyNibbles = N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key

deleteStorageKey'::Address->Word256->ContextM ()
deleteStorageKey' owner key = do
  addressState <- getAddressState owner
  db <- getStateDB
  let mpdb = db{MPDB.stateRoot=addressStateContractRoot addressState}
  newContractRoot <- fmap MPDB.stateRoot $ lift $ lift $ MPDB.deleteKey mpdb (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key)
  putAddressState owner addressState{addressStateContractRoot=newContractRoot}

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











