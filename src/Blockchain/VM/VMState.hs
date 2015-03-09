
module Blockchain.VM.VMState (
  VMState(..),
  Memory(..),
  startingState,
  VMException(..),
  addErr
--  getReturnValue
  ) where

import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.ByteString as B
import Data.IORef
import Data.Word


import Blockchain.Data.Address
import Blockchain.Data.AddressState
import Blockchain.Data.Code
import Blockchain.Data.Log
import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.VM.Code
import Blockchain.VM.Environment

data VMException =
  OutOfGasException  {eState::VMState} |
  StackTooSmallException {eState::VMState} |
  VMException {message::String, eState::VMState} |
  MalformedOpcodeException  {eState::VMState} |
  DivByZeroException  {eState::VMState} |
  InsufficientFunds  {eState::VMState} |
  AddressDoesNotExist {eState::VMState} |
  InvalidJump {eState::VMState}

instance Format VMException where
  format (OutOfGasException _) = "OutOfGasException"
  format (StackTooSmallException _) = "StackTooSmallException"
  format (VMException message _) = "VMException" ++ message
  format (MalformedOpcodeException _) = "MalformedOpcodeException"
  format (DivByZeroException _) = "DivByZeroException"
  format (InsufficientFunds _) = "InsufficientFunds"
  format (AddressDoesNotExist _) = "AddressDoesNotExist"
  format (InvalidJump _) = "InvalidJump"

addErr::String->Code->VMState->IO VMState
addErr message' c state = do
  let (op, _) = getOperationAt c (pc state)
  return state{vmException=Just $ VMException {message = message' ++ " for a call to " ++ show op, eState = state}}

data Memory =
  Memory {
    mVector::V.IOVector Word8,
    mSize::IORef Word256
    }
  

newMemory::IO Memory
newMemory = do
  arr <- V.new 100
  size <- newIORef 0
  forM_ [0..99] $ \p -> V.write arr p 0
  return $ Memory arr size

data VMState =
  VMState {
    vmGasRemaining::Integer,
    pc::Word256,
    memory::Memory,
    stack::[Word256],
    callDepth::Int,
    refund::Integer,
    
    suicideList::[Address],
    done::Bool,
    returnVal::Maybe B.ByteString,
    newAccounts::[(Maybe Address, Integer, AddressState)],
    
    logs::[Log],

    environment::Environment,
    
    vmException::Maybe VMException
    }


instance Format VMState where
  format state =
    "pc: " ++ show (pc state) ++ "\n" ++
    "done: " ++ show (done state) ++ "\n" ++
    "gasRemaining: " ++ show (vmGasRemaining state) ++ "\n" ++
    "stack: " ++ show (stack state) ++ "\n"

startingState::Environment->IO VMState
startingState env = do
  m <- newMemory
  return VMState 
             {
               pc = 0,
               done=False,
               returnVal=Nothing,
               vmException=Nothing,
               vmGasRemaining=0,
               stack=[],
               memory=m, 
               callDepth=0,
               refund=0,
               logs=[],
               environment=env,
               newAccounts=[],
               suicideList=[]
             }

{-
getReturnValue::VMState->IO B.ByteString
getReturnValue state = 
  case stack state of
    [add, size] -> mLoadByteString (memory state) add size
    [] -> return B.empty --Happens when STOP is called
    --TODO- This needs better error handling other than to just crash if the stack isn't 2 items long
    _ -> error "Error in getReturnValue: VM ended with stack in an unsupported case"

  
-}
