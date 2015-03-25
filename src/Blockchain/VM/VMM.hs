{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Blockchain.VM.VMM where

import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import qualified Data.ByteString as B

import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.Log
import qualified Blockchain.Database.MerklePatricia as MPDB
import Blockchain.DB.ModifyStateDB
import Blockchain.ExtWord
import Blockchain.Data.RLP
import Blockchain.DBM
import Blockchain.Util
import Blockchain.VM.Environment
import Blockchain.VM.VMState
import Blockchain.SHA
import qualified Data.NibbleString as N


type VMM = EitherT VMException (StateT VMState ContextM)


class Word256Storable a where
  fromWord256::Word256->a
  toWord256::a->Word256

instance Word256Storable Word256 where
  fromWord256 = id
  toWord256 = id

instance Word256Storable Address where
  fromWord256 h = Address $ fromIntegral (h `mod` (2^(160::Integer))::Word256)
  toWord256 (Address h) = fromIntegral h

instance Word256Storable SHA where
  fromWord256 h = SHA h
  toWord256 (SHA h) = h

instance Word256Storable Int where
  fromWord256 = fromIntegral
  toWord256 = fromIntegral

instance Word256Storable Integer where
  fromWord256 = fromIntegral
  toWord256 = fromIntegral

pop::Word256Storable a=>VMM a
pop = do
  state' <- lift get
  case state' of
    VMState{stack=val:rest} -> do
                lift $ put state'{stack=rest}
                return $ fromWord256 val
    _ -> left StackTooSmallException 


getStackItem::Word256Storable a=>Int->VMM a
getStackItem i = do
  state' <- lift get
  if length (stack state') > fromIntegral i
    then return $ fromWord256 (stack state' !! i)
    else left StackTooSmallException

push::Word256Storable a=>a->VMM ()
push val = do
  state' <- lift get
  lift $ put state'{stack = toWord256 val:stack state'}

addDebugCallCreate::DebugCallCreate->VMM ()
addDebugCallCreate callCreate = do
  state' <- lift $ get
  case debugCallCreates state' of
    Just x -> lift $ put state'{debugCallCreates = Just (callCreate:x)}
    Nothing -> error "You are trying to add a call create during a non-debug run"

addSuicideList::Address->VMM ()
addSuicideList address' = do
  state' <- lift get
  lift $ put state'{suicideList = address':suicideList state'}

getEnvVar::(Environment->a)->VMM a
getEnvVar f = do
  state' <- lift get
  return $ f $ environment state'

addLog::Log->VMM ()
addLog newLog = do
  state' <- lift get
  lift $ put state'{logs=newLog:logs state'}

setPC::Word256->VMM ()
setPC p = do
  state' <- lift get
  lift $ put state'{pc=p}

incrementPC::Word256->VMM ()
incrementPC p = do
  state' <- lift get
  lift $ put state'{pc=pc state' + p}

addToRefund::Integer->VMM ()
addToRefund val = do
  state' <- lift get
  lift $ put state'{refund=refund state' + val}

getCallDepth::VMM Int
getCallDepth = lift $ fmap callDepth $ get

getGasRemaining::VMM Integer
getGasRemaining = lift $ fmap vmGasRemaining $ get

setDone::Bool->VMM ()
setDone done' = do
  state' <- lift get
  lift $ put state'{done=done'}

setReturnVal::Maybe B.ByteString->VMM ()
setReturnVal returnVal' = do
  state' <- lift get
  lift $ put state'{returnVal=returnVal'}

setGasRemaining::Integer->VMM ()
setGasRemaining gasRemaining' = do
  state' <- lift get
  lift $ put state'{vmGasRemaining=gasRemaining'}

useGas::Integer->VMM ()
useGas gas = do
  state' <- lift get
  case vmGasRemaining state' - gas of
    x | x < 0 -> do
      lift $ put state'{vmGasRemaining=0}
      left OutOfGasException
    x -> lift $ put state'{vmGasRemaining=x}

addGas::Integer->VMM ()
addGas gas = do
  state' <- lift get
  case vmGasRemaining state' + gas of
    x | x < 0 -> left OutOfGasException
    x -> lift $ put state'{vmGasRemaining=x}

pay'::String->Address->Address->Integer->VMM ()
pay' reason from to val = do
  success <- lift $ lift $ pay reason from to val
  if success
    then return ()
    else left InsufficientFunds

addToBalance'::Address->Integer->VMM ()
addToBalance' address' val = do
  success <- lift $ lift $ addToBalance address' val
  if success
    then return ()
    else left InsufficientFunds

getStorageKeyVal::Word256->VMM Word256
getStorageKeyVal key = do
  owner <- getEnvVar envOwner
  lift $ lift $ getStorageKeyVal' owner key

getAllStorageKeyVals::VMM [(MPDB.Key, Word256)]
getAllStorageKeyVals = do
  owner <- getEnvVar envOwner
  lift $ lift $ getAllStorageKeyVals' owner


putStorageKeyVal::Word256->Word256->VMM ()
putStorageKeyVal key val = do
  owner <- getEnvVar envOwner
  lift $ lift $ putStorageKeyVal' owner key val

deleteStorageKey::Word256->VMM ()
deleteStorageKey key = do
  owner <- getEnvVar envOwner
  lift $ lift $ deleteStorageKey' owner key

