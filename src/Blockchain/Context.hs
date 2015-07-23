{-# LANGUAGE OverloadedStrings #-}

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


import Control.Monad.IfElse
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.NibbleString as N
import qualified Data.Vector as V
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.DBM
import Blockchain.Data.Peer
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.RLP
import qualified Blockchain.Database.MerklePatricia as MPDB
import qualified Blockchain.Database.MerklePatricia.Internal as MPDB
import Blockchain.ExtDBs
import Blockchain.ExtWord
import Blockchain.Options
import Blockchain.SHA

--import Debug.Trace

data Context =
  Context {
    neededBlockHashes::[SHA],
    pingCount::Int,
    peers::[Peer],
    miningDataset::B.ByteString,
    useAlternateGenesisBlock::Bool,
    vmTrace::[String]
    }

type ContextM = StateT Context DBM

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
  addressState <- lift $ getAddressState owner
  dbs <- lift get
  let mpdb = (stateDB dbs){MPDB.stateRoot=addressStateContractRoot addressState}
  maybeVal <- lift $ lift $ MPDB.getKeyVal mpdb (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key)
  case maybeVal of
    Nothing -> return 0
    Just x -> return $ fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode x

getAllStorageKeyVals'::Address->ContextM [(MPDB.Key, Word256)]
getAllStorageKeyVals' owner = do
  addressState <- lift $ getAddressState owner
  dbs <- lift get
  let mpdb = (stateDB dbs){MPDB.stateRoot=addressStateContractRoot addressState}
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
  lift $ hashDBPut storageKeyNibbles
  addressState <- lift $ getAddressState owner
  dbs <- lift get
  let mpdb = (stateDB dbs){MPDB.stateRoot=addressStateContractRoot addressState}
  newContractRoot <- fmap MPDB.stateRoot $ lift $ lift $ MPDB.putKeyVal mpdb storageKeyNibbles (rlpEncode $ rlpSerialize $ rlpEncode $ toInteger val)
  lift $ putAddressState owner addressState{addressStateContractRoot=newContractRoot}
  where storageKeyNibbles = N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key

deleteStorageKey'::Address->Word256->ContextM ()
deleteStorageKey' owner key = do
  addressState <- lift $ getAddressState owner
  dbs <- lift get
  let mpdb = (stateDB dbs){MPDB.stateRoot=addressStateContractRoot addressState}
  newContractRoot <- fmap MPDB.stateRoot $ lift $ lift $ MPDB.deleteKey mpdb (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key)
  lift $ putAddressState owner addressState{addressStateContractRoot=newContractRoot}

incrementNonce::Address->ContextM ()
incrementNonce address = do
  addressState <- lift $ getAddressState address
  lift $ putAddressState address addressState{ addressStateNonce = addressStateNonce addressState + 1 }

getNewAddress::Address->ContextM Address
getNewAddress address = do
  addressState <- lift $ getAddressState address
  when flags_debug $ liftIO $ putStrLn $ "Creating new account: owner=" ++ show (pretty address) ++ ", nonce=" ++ show (addressStateNonce addressState)
  let newAddress = getNewAddress_unsafe address (addressStateNonce addressState)
  incrementNonce address
  return newAddress











