
module ExtDBs (
  MP.SHAPtr(..),
  MP.emptyTriePtr,
  detailsDBPut,
  detailsDBGet,
  blockDBGet,
  blockDBPut,
  codeDBGet,
  codeDBPut,
  stateDBPut,
  stateDBGet,
  putKeyVal,
  getKeyVals,
  deleteStorageKey,
  putStorageKeyVal,
  getStorageKeyVals
  ) where

import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Binary hiding (get, put)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default
import qualified Database.LevelDB as DB

import qualified Data.NibbleString as N
import SHA
import Data.RLP
import qualified Database.MerklePatricia as MP

import Context

detailsDBPut::B.ByteString->B.ByteString->ContextM ()
detailsDBPut key val = do
  ctx <- get
  runResourceT $ 
    DB.put (detailsDB ctx) def key val
    
detailsDBGet::B.ByteString->ContextM (Maybe B.ByteString)
detailsDBGet key = do
  ctx <- get
  runResourceT $ 
    DB.get (detailsDB ctx) def key
    
blockDBPut::B.ByteString->B.ByteString->ContextM ()
blockDBPut key val = do
  ctx <- get
  runResourceT $ 
    DB.put (blockDB ctx) def key val
    
blockDBGet::B.ByteString->ContextM (Maybe B.ByteString)
blockDBGet key = do
  ctx <- get
  runResourceT $ 
    DB.get (blockDB ctx) def key
    

codeDBPut::B.ByteString->ContextM ()
codeDBPut code = do
  ctx <- get
  runResourceT $ 
    DB.put (codeDB ctx) def (BL.toStrict $ encode $ hash code) code
    

codeDBGet::B.ByteString->ContextM (Maybe B.ByteString)
codeDBGet key = do
  ctx <- get
  runResourceT $ 
    DB.get (codeDB ctx) def key
    
stateDBPut::B.ByteString->B.ByteString->ContextM ()
stateDBPut key val = do
  ctx <- get
  runResourceT $ 
    DB.put (MP.ldb $ stateDB ctx) def key val
  put ctx{stateDB=(stateDB ctx){MP.stateRoot=MP.SHAPtr key}}

stateDBGet::B.ByteString->ContextM (Maybe B.ByteString)
stateDBGet key = do
  ctx <- get
  runResourceT $ 
    DB.get (MP.ldb $ stateDB ctx) def key
    

putKeyVal::N.NibbleString->RLPObject->ContextM ()
putKeyVal key val = do
  ctx <- get
  newStateDB <-
    liftIO $ runResourceT $ MP.putKeyVal (stateDB ctx) key val
  put ctx{stateDB=newStateDB}

getKeyVals::N.NibbleString->ContextM [(N.NibbleString, RLPObject)]
getKeyVals key = do
  ctx <- get
  liftIO $ runResourceT $ MP.getKeyVals (stateDB ctx) key

deleteStorageKey::N.NibbleString->ContextM ()
deleteStorageKey key = do
  ctx <- get
  newStorageDB <-
    liftIO $ runResourceT $ MP.deleteKey (storageDB ctx) key
  put ctx{storageDB=newStorageDB}

putStorageKeyVal::N.NibbleString->RLPObject->ContextM ()
putStorageKeyVal key val = do
  ctx <- get
  newStorageDB <-
    liftIO $ runResourceT $ MP.putKeyVal (storageDB ctx) key val
  put ctx{storageDB=newStorageDB}

getStorageKeyVals::N.NibbleString->ContextM [(N.NibbleString, RLPObject)]
getStorageKeyVals key = do
  ctx <- get
  liftIO $ runResourceT $ MP.getKeyVals (storageDB ctx) key
