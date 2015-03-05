
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

data VMException = OutOfGasException | StackTooSmallException | VMException String | MalformedOpcodeException | DivByZeroException | InsufficientFunds | InvalidJump deriving (Show)

addErr::String->Code->VMState->IO VMState
addErr message c state = do
  let (op, _) = getOperationAt c (pc state)
  return state{vmException=Just $ VMException (message ++ " for a call to " ++ show op)}

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
    
    vmException::Maybe VMException
    }


instance Format VMState where
  format state =
    "pc: " ++ show (pc state) ++ "\n" ++
    "done: " ++ show (done state) ++ "\n" ++
    "gasRemaining: " ++ show (vmGasRemaining state) ++ "\n" ++
    "stack: " ++ show (stack state) ++ "\n"

startingState::IO VMState
startingState = do
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
