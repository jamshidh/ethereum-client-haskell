
module VMState (
  VMState(..),
  startingState,
  VMError,
  addErr,
  getReturnValue
  ) where

import qualified Data.ByteString as B
import Data.Functor
import qualified Data.Map as M

import Code
import ExtWord
import Format
import Memory

data VMError = VMError String deriving (Show)

addErr::String->Code->VMState->IO VMState
addErr message c state = do
  let (op, _) = getOperationAt c (pc state)
  return state{vmError=Just $ VMError (message ++ " for a call to " ++ show op)}

data VMState =
  VMState {
    pc::Int,
    done::Bool,
    vmError::Maybe VMError,
    vmGasRemaining::Integer,
    stack::[Word256],
    memory::Memory,
    storage::M.Map Word256 Word256,
    markedForSuicide::Bool
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
  return VMState {
    pc = 0,
    done=False,
    vmError=Nothing,
    vmGasRemaining=0,
    stack=[],
    memory=m, storage = M.empty, markedForSuicide=False }


getReturnValue::VMState->IO (Either VMError B.ByteString)
getReturnValue state = do
  case (vmError state, stack state) of
    (Just err, _) -> return $ Left err
    (Nothing, [add, size]) -> Right <$> mLoadByteString (memory state) add size
    (Nothing, []) -> return $ Right B.empty --Happens when STOP is called
      --TODO- This needs better error handling other than to just crash if the stack isn't 2 items long
    _ -> error $ "Error in getReturnValue: VM ended with stack in an unsupported case"

  
