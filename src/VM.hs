
module VM where

import Prelude hiding (LT, GT, EQ)

import Data.Bits
import qualified Data.ByteString as B
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Database.LevelDB
import Network.Haskoin.Crypto (Word256)

import Address
import AddressState
import Code
import DBs
import Format
import Memory
import Opcodes
import SHA
import Util

--import Debug.Trace




data VMError = VMError String deriving (Show)

data VMState =
  VMState {
    code::Code,
    pc::Int,
    done::Bool,
    vmError::Maybe VMError,
    vmGasRemaining::Integer,
    vars::M.Map String String,
    stack::[Word256],
    --memory::IOArray Integer Word8,
    memory::Memory,
    storage::M.Map Word256 Word256,
    address::Address
    }

instance Format VMState where
  format state =
    "pc: " ++ show (pc state) ++ "\n" ++
    "done: " ++ show (done state) ++ "\n" ++
    "gasRemaining: " ++ show (vmGasRemaining state) ++ "\n" ++
    "stack: " ++ show (stack state) ++ "\n"

startingState::Code->Address->IO VMState
startingState c a = do
  m <- newMemory
  return VMState { code = c, pc = 0, done=False, vmError=Nothing, vmGasRemaining=0, vars=M.empty, stack=[], memory=m, storage = M.empty, address = a }








--SHA3 | ADDRESS | BALANCE | ORIGIN | CALLER | CALLVALUE | CALLDATALOAD | CALLDATASIZE | CALLDATACOPY | CODESIZE | CODECOPY | GASPRICE | PREVHASH | COINBASE | TIMESTAMP | NUMBER | DIFFICULTY | GASLIMIT | POP | DUP | SWAP | MLOAD | MSTORE | MSTORE8 | SLOAD | SSTORE | JUMP | JUMPI | PC | MSIZE | GAS

--               | PUSH1 Word8 | PUSH2 | PUSH3 | PUSH4 | PUSH5 | PUSH6 | PUSH7 | PUSH8 | PUSH9 | PUSH10 | PUSH11 | PUSH12 | PUSH13 | PUSH14 | PUSH15 | PUSH16 | PUSH17 | PUSH18 | PUSH19 | PUSH20 | PUSH21 | PUSH22 | PUSH23 | PUSH24 | PUSH25 | PUSH26 | PUSH27 | PUSH28 | PUSH29 | PUSH30 | PUSH31 | PUSH32

--               | CREATE | CALL | RETURN | SUICIDE deriving (Show, Eq, Ord)



bool2Word256::Bool->Word256
bool2Word256 True = 1
bool2Word256 False = 0

word2562Bool::Word256->Bool
word2562Bool 1 = True
word2562Bool _ = False

addErr::String->VMState->IO VMState
addErr message state = do
  let (op, _) = getOperationAt (code state) (pc state)
  return state{vmError=Just $ VMError (message ++ " for a call to " ++ show op)}

binaryAction::(Word256->Word256->Word256)->VMState->IO VMState
binaryAction action state@VMState{stack=(x:y:rest)} = return state{stack=(x `action` y:rest)}
binaryAction _ state = addErr "stack did not contain enough elements" state

unaryAction::(Word256->Word256)->VMState->IO VMState
unaryAction action state@VMState{stack=(x:rest)} = return state{stack=(action x:rest)}
unaryAction _ state = addErr "stack did not contain enough elements" state



runOperation::StateDB->SHAPtr->Operation->VMState->IO VMState
runOperation _ _ STOP state = return state{done=True}

runOperation _ _ ADD state = binaryAction (+) state
runOperation _ _ MUL state = binaryAction (*) state
runOperation _ _ SUB state = binaryAction (-) state
runOperation _ _ DIV state = binaryAction quot state
runOperation _ _ SDIV state = binaryAction undefined state
runOperation _ _ MOD state = binaryAction mod state
runOperation _ _ SMOD state = binaryAction undefined state
runOperation _ _ EXP state = binaryAction (^) state
runOperation _ _ NEG state = unaryAction negate state
runOperation _ _ LT state = binaryAction ((bool2Word256 .) . (<)) state
runOperation _ _ GT state = binaryAction ((bool2Word256 .) . (>)) state
runOperation _ _ SLT state = binaryAction undefined state
runOperation _ _ SGT state = binaryAction undefined state
runOperation _ _ EQ state = binaryAction ((bool2Word256 .) . (==)) state
runOperation _ _ NOT state = unaryAction (bool2Word256 . not . word2562Bool) state
runOperation _ _ AND state = binaryAction (.&.) state
runOperation _ _ OR state = binaryAction (.|.) state
runOperation _ _ XOR state = binaryAction xor state

runOperation _ _ BYTE state = binaryAction (\x y -> y `shiftR` fromIntegral x .&. 0xFF) state

runOperation _ _ SHA3 state@VMState{stack=(p:size:rest)} = do
  SHA theHash <- hash <$> mLoadByteString (memory state) p size
  return state{stack=theHash:rest}

runOperation _ _ ADDRESS state = return state{stack=fromIntegral a:stack state}
    where
      Address a = address state

runOperation sdb p BALANCE state@VMState{stack=(x:rest)} = do
  maybeAddressState <- runResourceT $ do
                    getAddressState sdb p (Address $ fromIntegral x)
  return state{stack=(fromIntegral $ fromMaybe 0 $ balance <$> maybeAddressState):rest}
runOperation _ _ BALANCE state = addErr "stack did not contain enough elements" state

--ORIGIN | CALLER | CALLVALUE | CALLDATALOAD | CALLDATASIZE | CALLDATACOPY | CODESIZE | CODECOPY | GASPRICE | PREVHASH | COINBASE | TIMESTAMP | NUMBER | DIFFICULTY | GASLIMIT | 

runOperation _ _ POP state@VMState{stack=_:rest} = return state{stack=rest}
runOperation _ _ POP state = addErr "Stack did not contain any items" state

runOperation _ _ DUP state@VMState{stack=x:rest} = return state{stack=x:x:rest}
runOperation _ _ DUP state = addErr "Stack did not contain any items" state

runOperation _ _ SWAP state@VMState{stack=x:y:rest} = return state{stack=y:x:rest}
runOperation _ _ SWAP state = addErr "Stack did not contain enough items" state

runOperation _ _ MLOAD state@VMState{stack=(p:rest)} = do
  bytes <- mLoad (memory state) p --sequence $ readArray (memory state) <$> fromIntegral <$> [p..p+31]
  return $ state { stack=fromInteger (bytes2Integer bytes):rest }
  
runOperation _ _ MSTORE state@VMState{stack=(p:val:rest)} = do
  mStore (memory state) p val
  return state{stack=rest}

runOperation _ _ MSTORE8 state@VMState{stack=(p:val:rest)} = do
  mStore8 (memory state) (fromIntegral p) (fromIntegral $ val .&. 0xFF)
  return $
    state { stack=rest }

runOperation _ _ SLOAD state@VMState{stack=(p:rest)} = do
  let val = fromMaybe 0 $ M.lookup p (storage state)
  return $ state { stack=val:rest }
  
runOperation _ _ SSTORE state@VMState{stack=(p:val:rest)} = do
  return $ state { stack=rest, storage=M.insert p val $ storage state }

runOperation _ _ JUMP state@VMState{stack=(p:rest)} =
  return $ state { stack=rest, pc=fromIntegral p }

runOperation _ _ JUMPI state@VMState{stack=(p:cond:rest)} =
  return $ state { stack=rest, pc=if word2562Bool cond then fromIntegral p else (pc state) }

runOperation _ _ PC state =
  return state{stack=fromIntegral (pc state):stack state}

runOperation _ _ MSIZE state@VMState{memory=Memory mSize _} =
  return state{stack=mSize:stack state}

runOperation _ _ GAS state =
  return $ state { stack=fromInteger (vmGasRemaining state):stack state }

runOperation _ _ (PUSH vals) state =
  return $
  state { stack=(fromIntegral <$> vals) ++ stack state }
runOperation _ _ RETURN state =
  return $ state { done=True }
runOperation _ _ x _ = error $ "Missing case in runOperation: " ++ show x

movePC::VMState->Int->VMState
movePC state l = state{ pc=pc state + l }

decreaseGas::Operation->VMState->VMState
decreaseGas STOP state = state
decreaseGas MSTORE state = state{ vmGasRemaining = vmGasRemaining state - 2 }
decreaseGas _ state = state{ vmGasRemaining = vmGasRemaining state - 1 }


runCode::StateDB->SHAPtr->VMState->IO VMState
runCode sdb p state = do
  let (op, len) = getOperationAt (code state) (pc state)
  result <- runOperation sdb p op state
  case result of
    VMState{vmError=Just _} -> return result
    VMState{done=True} -> return $ decreaseGas op $ movePC result len
    state2 -> runCode sdb p $ decreaseGas op $ movePC state2 len

getReturnValue::VMState->IO (Either VMError B.ByteString)
getReturnValue state = do
  case vmError state of
    Just err -> return $ Left err
    Nothing -> do
      --TODO- This needs better error handling other than to just crash if the stack isn't 2 items long
      vals <- 
        case stack state of
          [add, size] ->
            sequence $ mLoad8 (memory state) <$> fromIntegral <$> [add..add+size-1]
          [] -> return [] --Happens when STOP is called
          _ -> error $ "Error in getReturnValue: VM ended with stack in an unsupported case"
      return $ Right $ B.pack vals
  


runCodeFromStart::StateDB->SHAPtr->Code->Address->IO VMState
runCodeFromStart sdb p rom add = do
  s <- startingState rom add
  runCode sdb p s
