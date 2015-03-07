{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Blockchain.VM.VMM where

import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import qualified Data.ByteString as B

import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressState
import Blockchain.Data.Log
import Blockchain.ExtWord
import Blockchain.VM.Environment
import Blockchain.VM.VMState
import Blockchain.SHA

type VMM = EitherT VMException (StateT VMState ContextM)


class Word256Storable a where
  fromWord256::Word256->a
  toWord256::a->Word256

instance Word256Storable Word256 where
  fromWord256 = id
  toWord256 = id

instance Word256Storable Address where
  fromWord256 h = Address $ fromIntegral h
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
    _ -> left $ StackTooSmallException state'


push::Word256Storable a=>a->VMM ()
push val = do
  state' <- lift get
  lift $ put state'{stack = toWord256 val:stack state'}

addNewAccount::(Maybe Address, Integer, AddressState)->VMM ()
addNewAccount newAccount = do
  state' <- lift get
  lift $ put state'{newAccounts = newAccount:newAccounts state'}

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
  lift $ put state'{vmGasRemaining=vmGasRemaining state' - gas}