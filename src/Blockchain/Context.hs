{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Context (
  Context(..),
  ContextM,
  initContext,
  isDebugEnabled,
  getStorageKeyVal',
  getAllStorageKeyVals',
  putStorageKeyVal',
  deleteStorageKey',
  incrementNonce,
  getNewAddress
  ) where


import Control.Monad.IfElse
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import System.Directory
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Constants
import Blockchain.DBM
import Blockchain.Data.Peer
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import qualified Blockchain.Database.MerklePatricia as MPDB
import Blockchain.ExtWord
import Blockchain.SHA
import Blockchain.Util
import qualified Data.NibbleString as N

--import Debug.Trace

data Context =
  Context {
    neededBlockHashes::[SHA],
    pingCount::Int,
    peers::[Peer],
    debugEnabled::Bool
    }

type ContextM = StateT Context DBM

isDebugEnabled::ContextM Bool
isDebugEnabled = do
  cxt <- get
  return $ debugEnabled cxt 

initContext::String->ResourceT IO Context
initContext theType = do
  homeDir <- liftIO getHomeDirectory                     
  liftIO $ createDirectoryIfMissing False $ homeDir </> dbDir theType
  return $ Context
      []
      0
      []
      False

getStorageKeyVal'::Address->Word256->ContextM Word256
getStorageKeyVal' owner key = do
  addressState <- lift $ getAddressState owner
  dbs <- lift get
  let mpdb = (stateDB dbs){MPDB.stateRoot=addressStateContractRoot addressState}
  vals <- lift $ lift $ MPDB.getKeyVals mpdb (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key)
  case vals of
    [] -> return 0
    [x] -> return $ fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode $ snd x
    _ -> error "Multiple values in storage"

getAllStorageKeyVals'::Address->ContextM [(MPDB.Key, Word256)]
getAllStorageKeyVals' owner = do
  addressState <- lift $ getAddressState owner
  dbs <- lift get
  let mpdb = (stateDB dbs){MPDB.stateRoot=addressStateContractRoot addressState}
  kvs <- lift $ lift $ MPDB.unsafeGetKeyVals mpdb ""
  return $ map (fmap $ fromInteger . rlpDecode . rlpDeserialize . rlpDecode) kvs

putStorageKeyVal'::Address->Word256->Word256->ContextM ()
putStorageKeyVal' owner key val = do
  addressState <- lift $ getAddressState owner
  dbs <- lift get
  let mpdb = (stateDB dbs){MPDB.stateRoot=addressStateContractRoot addressState}
  newContractRoot <- fmap MPDB.stateRoot $ lift $ lift $ MPDB.putKeyVal mpdb (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes key) (rlpEncode $ rlpSerialize $ rlpEncode $ toInteger val)
  lift $ putAddressState owner addressState{addressStateContractRoot=newContractRoot}

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
  whenM isDebugEnabled $ liftIO $ putStrLn $ "Creating new account: owner=" ++ show (pretty address) ++ ", nonce=" ++ show (addressStateNonce addressState)
  let newAddress = getNewAddress_unsafe address (addressStateNonce addressState)
  incrementNonce address
  return newAddress











