
module VM (
  runCodeFromStart
  ) where

import Prelude hiding (LT, GT, EQ)

import Data.Bits
import qualified Data.ByteString as B
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock.POSIX
import Database.LevelDB
import Network.Haskoin.Crypto (Word256)

import Address
import AddressState
import Block
import Code
import DBs
import Environment
import Memory
import Opcodes
import SHA
import Util
import VMState

--import Debug.Trace

bool2Word256::Bool->Word256
bool2Word256 True = 1
bool2Word256 False = 0

word2562Bool::Word256->Bool
word2562Bool 1 = True
word2562Bool _ = False

binaryAction::(Word256->Word256->Word256)->Environment->VMState->IO VMState
binaryAction action _ state@VMState{stack=(x:y:rest)} = return state{stack=(x `action` y:rest)}
binaryAction _ env state = addErr "stack did not contain enough elements" (envCode env) state

unaryAction::(Word256->Word256)->Environment->VMState->IO VMState
unaryAction action _ state@VMState{stack=(x:rest)} = return state{stack=(action x:rest)}
unaryAction _ env state = addErr "stack did not contain enough elements" (envCode env) state



runOperation::StateDB->SHAPtr->Operation->Environment->VMState->IO VMState
runOperation _ _ STOP _ state = return state{done=True}

runOperation _ _ ADD env state = binaryAction (+) env state
runOperation _ _ MUL env state = binaryAction (*) env state
runOperation _ _ SUB env state = binaryAction (-) env state
runOperation _ _ DIV env state = binaryAction quot env state
runOperation _ _ SDIV env state = binaryAction undefined env state
runOperation _ _ MOD env state = binaryAction mod env state
runOperation _ _ SMOD env state = binaryAction undefined env state
runOperation _ _ EXP env state = binaryAction (^) env state
runOperation _ _ NEG env state = unaryAction negate env state
runOperation _ _ LT env state = binaryAction ((bool2Word256 .) . (<)) env state
runOperation _ _ GT env state = binaryAction ((bool2Word256 .) . (>)) env state
runOperation _ _ SLT env state = binaryAction undefined env state
runOperation _ _ SGT env state = binaryAction undefined env state
runOperation _ _ EQ env state = binaryAction ((bool2Word256 .) . (==)) env state
runOperation _ _ NOT env state = unaryAction (bool2Word256 . not . word2562Bool) env state
runOperation _ _ AND env state = binaryAction (.&.) env state
runOperation _ _ OR env state = binaryAction (.|.) env state
runOperation _ _ XOR env state = binaryAction xor env state

runOperation _ _ BYTE env state = binaryAction (\x y -> y `shiftR` fromIntegral x .&. 0xFF) env state

runOperation _ _ SHA3 _ state@VMState{stack=(p:size:rest)} = do
  SHA theHash <- hash <$> mLoadByteString (memory state) p size
  return state{stack=theHash:rest}

runOperation _ _ ADDRESS Environment{envOwner=Address a} state = return state{stack=fromIntegral a:stack state}

runOperation sdb p BALANCE _ state@VMState{stack=(x:rest)} = do
  maybeAddressState <- runResourceT $ do
                    getAddressState sdb p (Address $ fromIntegral x)
  return state{stack=(fromIntegral $ fromMaybe 0 $ balance <$> maybeAddressState):rest}
runOperation _ _ BALANCE env state = addErr "stack did not contain enough elements" (envCode env) state

runOperation _ _ ORIGIN Environment{envSender=Address sender} state = return state{stack=fromIntegral sender:stack state}

runOperation _ _ CALLER Environment{envOwner=Address owner} state = return state{stack=fromIntegral owner:stack state}

runOperation _ _ CALLVALUE Environment{envValue=val} state = return state{stack=fromIntegral val:stack state}

runOperation _ _ CALLDATALOAD Environment{envInputData=d} state@VMState{stack=p:rest} = do
  let val = bytes2Integer $ B.unpack $ B.take 32 $ B.drop (fromIntegral p) d
  return state{stack=fromIntegral val:rest}

runOperation _ _ CALLDATASIZE Environment{envInputData=d} state = return state{stack=fromIntegral (B.length d):stack state}

runOperation _ _ CALLDATACOPY Environment{envInputData=d} state@VMState{stack=memP:codeP:size:rest} = do
  mStoreByteString (memory state) memP $ B.take (fromIntegral size) $ B.drop (fromIntegral codeP) d
  return state{stack=rest}

runOperation _ _ CODESIZE Environment{envCode=c} state = return state{stack=fromIntegral (codeLength c):stack state}

runOperation _ _ CODECOPY Environment{envCode=Code c} state@VMState{stack=memP:codeP:size:rest} = do
  mStoreByteString (memory state) memP $ B.take (fromIntegral size) $ B.drop (fromIntegral codeP) c
  return state{stack=rest}

runOperation _ _ GASPRICE Environment{envGasPrice=gp} state = return state{stack=fromIntegral gp:stack state}

runOperation _ _ PREVHASH Environment{envBlock=Block{blockData=BlockData{parentHash=SHA prevHash}}} state = return state{stack=prevHash:stack state}

runOperation _ _ COINBASE Environment{envBlock=Block{blockData=BlockData{coinbase=Address cb}}} state = return state{stack=fromIntegral cb:stack state}

runOperation _ _ TIMESTAMP Environment{envBlock=Block{blockData=bd}} state = return state{stack=(round $ utcTimeToPOSIXSeconds $ timestamp bd):stack state}
runOperation _ _ NUMBER Environment{envBlock=Block{blockData=bd}} state = return state{stack=fromIntegral (number bd):stack state}
runOperation _ _ DIFFICULTY Environment{envBlock=Block{blockData=bd}} state = return state{stack=fromIntegral (difficulty bd):stack state}
runOperation _ _ GASLIMIT Environment{envBlock=Block{blockData=bd}} state = return state{stack=fromIntegral (gasLimit bd):stack state}

runOperation _ _ POP _ state@VMState{stack=_:rest} = return state{stack=rest}
runOperation _ _ POP env state = addErr "Stack did not contain any items" (envCode env) state

runOperation _ _ DUP _ state@VMState{stack=x:rest} = return state{stack=x:x:rest}
runOperation _ _ DUP env state = addErr "Stack did not contain any items" (envCode env) state

runOperation _ _ SWAP _ state@VMState{stack=x:y:rest} = return state{stack=y:x:rest}
runOperation _ _ SWAP env state = addErr "Stack did not contain enough items" (envCode env) state

runOperation _ _ MLOAD _ state@VMState{stack=(p:rest)} = do
  bytes <- mLoad (memory state) p --sequence $ readArray (memory state) <$> fromIntegral <$> [p..p+31]
  return $ state { stack=fromInteger (bytes2Integer bytes):rest }
  
runOperation _ _ MSTORE _ state@VMState{stack=(p:val:rest)} = do
  mStore (memory state) p val
  return state{stack=rest}

runOperation _ _ MSTORE8 _ state@VMState{stack=(p:val:rest)} = do
  mStore8 (memory state) (fromIntegral p) (fromIntegral $ val .&. 0xFF)
  return $
    state { stack=rest }

runOperation _ _ SLOAD _ state@VMState{stack=(p:rest)} = do
  let val = fromMaybe 0 $ M.lookup p (storage state)
  return $ state { stack=val:rest }
  
runOperation _ _ SSTORE _ state@VMState{stack=(p:val:rest)} = do
  return $ state { stack=rest, storage=M.insert p val $ storage state }

runOperation _ _ JUMP _ state@VMState{stack=(p:rest)} =
  return $ state { stack=rest, pc=fromIntegral p }

runOperation _ _ JUMPI _ state@VMState{stack=(p:cond:rest)} =
  return $ state { stack=rest, pc=if word2562Bool cond then fromIntegral p else (pc state) }

runOperation _ _ PC _ state =
  return state{stack=fromIntegral (pc state):stack state}

runOperation _ _ MSIZE _ state@VMState{memory=Memory mSize _} =
  return state{stack=mSize:stack state}

runOperation _ _ GAS _ state =
  return $ state { stack=fromInteger (vmGasRemaining state):stack state }

runOperation _ _ (PUSH vals) _ state =
  return $
  state { stack=(fromIntegral <$> vals) ++ stack state }



--               | CREATE | CALL | RETURN | SUICIDE deriving (Show, Eq, Ord)



runOperation _ _ RETURN _ state =
  return $ state { done=True }

runOperation _ _ SUICIDE _ state =
  return $ state { done=True, markedForSuicide=True }

runOperation _ _ x _ _ = error $ "Missing case in runOperation: " ++ show x



-------------------




movePC::VMState->Int->VMState
movePC state l = state{ pc=pc state + l }

decreaseGas::Operation->VMState->VMState
decreaseGas STOP state = state
decreaseGas MSTORE state = state{ vmGasRemaining = vmGasRemaining state - 2 }
decreaseGas _ state = state{ vmGasRemaining = vmGasRemaining state - 1 }


runCode::StateDB->SHAPtr->Environment->VMState->IO VMState
runCode sdb p env state = do
  let (op, len) = getOperationAt (envCode env) (pc state)
  result <- runOperation sdb p op env state
  case result of
    VMState{vmError=Just _} -> return result
    VMState{done=True} -> return $ decreaseGas op $ movePC result len
    state2 -> runCode sdb p env $ decreaseGas op $ movePC state2 len

runCodeFromStart::StateDB->SHAPtr->Environment->IO VMState
runCodeFromStart sdb p env = do
  runCode sdb p env =<< startingState

