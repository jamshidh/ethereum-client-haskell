{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Context (
  Context(..),
  ContextM,
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
import qualified Data.ByteString as B
import qualified Data.NibbleString as N
import qualified Data.Vector as V
import System.Directory
import System.FilePath
import System.IO
import System.IO.MMap
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Constants
import Blockchain.DBM
import Blockchain.Data.Peer
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import qualified Blockchain.Database.MerklePatricia as MPDB
import qualified Blockchain.Database.MerklePatricia.Internal as MPDB
import Blockchain.ExtDBs
import Blockchain.ExtWord
import Blockchain.SHA
import Blockchain.Util
import Cache
import Constants
import qualified Data.NibbleString as N

--import Debug.Trace

data Context =
  Context {
    neededBlockHashes::[SHA],
    pingCount::Int,
    peers::[Peer],
    miningDataset::B.ByteString,
    useAlternateGenesisBlock::Bool,
    debugEnabled::Bool
    }

type ContextM = StateT Context DBM

isDebugEnabled::ContextM Bool
isDebugEnabled = do
  cxt <- get
  return $ debugEnabled cxt 

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
  whenM isDebugEnabled $ liftIO $ putStrLn $ "Creating new account: owner=" ++ show (pretty address) ++ ", nonce=" ++ show (addressStateNonce addressState)
  let newAddress = getNewAddress_unsafe address (addressStateNonce addressState)
  incrementNonce address
  return newAddress











