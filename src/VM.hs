
module VM (
  runCodeFromStart
  ) where

import Prelude hiding (LT, GT, EQ)

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.ByteString as B
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock.POSIX
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



runOperation::DB->Operation->Environment->VMState->IO VMState
runOperation _ STOP _ state = return state{done=True}

runOperation _ ADD env state = binaryAction (+) env state
runOperation _ MUL env state = binaryAction (*) env state
runOperation _ SUB env state = binaryAction (-) env state
runOperation _ DIV env state = binaryAction quot env state
runOperation _ SDIV env state = binaryAction undefined env state
runOperation _ MOD env state = binaryAction mod env state
runOperation _ SMOD env state = binaryAction undefined env state
runOperation _ EXP env state = binaryAction (^) env state
runOperation _ NEG env state = unaryAction negate env state
runOperation _ LT env state = binaryAction ((bool2Word256 .) . (<)) env state
runOperation _ GT env state = binaryAction ((bool2Word256 .) . (>)) env state
runOperation _ SLT env state = binaryAction undefined env state
runOperation _ SGT env state = binaryAction undefined env state
runOperation _ EQ env state = binaryAction ((bool2Word256 .) . (==)) env state
runOperation _ NOT env state = unaryAction (bool2Word256 . not . word2562Bool) env state
runOperation _ AND env state = binaryAction (.&.) env state
runOperation _ OR env state = binaryAction (.|.) env state
runOperation _ XOR env state = binaryAction xor env state

runOperation _ BYTE env state = binaryAction (\x y -> y `shiftR` fromIntegral x .&. 0xFF) env state

runOperation _ SHA3 _ state@VMState{stack=(p:size:rest)} = do
  SHA theHash <- hash <$> mLoadByteString (memory state) p size
  return state{stack=theHash:rest}

runOperation _ ADDRESS Environment{envOwner=Address a} state = return state{stack=fromIntegral a:stack state}

runOperation db BALANCE _ state@VMState{stack=(x:rest)} = do
  maybeAddressState <- runResourceT $ do
                    getAddressState db (Address $ fromIntegral x)
  return state{stack=(fromIntegral $ fromMaybe 0 $ balance <$> maybeAddressState):rest}
runOperation _ BALANCE env state = addErr "stack did not contain enough elements" (envCode env) state

runOperation _ ORIGIN Environment{envSender=Address sender} state = return state{stack=fromIntegral sender:stack state}

runOperation _ CALLER Environment{envOwner=Address owner} state = return state{stack=fromIntegral owner:stack state}

runOperation _ CALLVALUE Environment{envValue=val} state = return state{stack=fromIntegral val:stack state}

runOperation _ CALLDATALOAD Environment{envInputData=d} state@VMState{stack=p:rest} = do
  let val = bytes2Integer $ B.unpack $ B.take 32 $ B.drop (fromIntegral p) d
  return state{stack=fromIntegral val:rest}

runOperation _ CALLDATASIZE Environment{envInputData=d} state = return state{stack=fromIntegral (B.length d):stack state}

runOperation _ CALLDATACOPY Environment{envInputData=d} state@VMState{stack=memP:codeP:size:rest} = do
  mStoreByteString (memory state) memP $ B.take (fromIntegral size) $ B.drop (fromIntegral codeP) d
  return state{stack=rest}

runOperation _ CODESIZE Environment{envCode=c} state = return state{stack=fromIntegral (codeLength c):stack state}

runOperation _ CODECOPY Environment{envCode=Code c} state@VMState{stack=memP:codeP:size:rest} = do
  mStoreByteString (memory state) memP $ B.take (fromIntegral size) $ B.drop (fromIntegral codeP) c
  return state{stack=rest}

runOperation _ GASPRICE Environment{envGasPrice=gp} state = return state{stack=fromIntegral gp:stack state}

runOperation _ PREVHASH Environment{envBlock=Block{blockData=BlockData{parentHash=SHA prevHash}}} state = return state{stack=prevHash:stack state}

runOperation _ COINBASE Environment{envBlock=Block{blockData=BlockData{coinbase=Address cb}}} state = return state{stack=fromIntegral cb:stack state}

runOperation _ TIMESTAMP Environment{envBlock=Block{blockData=bd}} state = return state{stack=(round $ utcTimeToPOSIXSeconds $ timestamp bd):stack state}
runOperation _ NUMBER Environment{envBlock=Block{blockData=bd}} state = return state{stack=fromIntegral (number bd):stack state}
runOperation _ DIFFICULTY Environment{envBlock=Block{blockData=bd}} state = return state{stack=fromIntegral (difficulty bd):stack state}
runOperation _ GASLIMIT Environment{envBlock=Block{blockData=bd}} state = return state{stack=fromIntegral (gasLimit bd):stack state}

runOperation _ POP _ state@VMState{stack=_:rest} = return state{stack=rest}
runOperation _ POP env state = addErr "Stack did not contain any items" (envCode env) state

runOperation _ DUP _ state@VMState{stack=x:rest} = return state{stack=x:x:rest}
runOperation _ DUP env state = addErr "Stack did not contain any items" (envCode env) state

runOperation _ SWAP _ state@VMState{stack=x:y:rest} = return state{stack=y:x:rest}
runOperation _ SWAP env state = addErr "Stack did not contain enough items" (envCode env) state

runOperation _ MLOAD _ state@VMState{stack=(p:rest)} = do
  bytes <- mLoad (memory state) p --sequence $ readArray (memory state) <$> fromIntegral <$> [p..p+31]
  return $ state { stack=fromInteger (bytes2Integer bytes):rest }
  
runOperation _ MSTORE _ state@VMState{stack=(p:val:rest)} = do
  mStore (memory state) p val
  return state{stack=rest}

runOperation _ MSTORE8 _ state@VMState{stack=(p:val:rest)} = do
  mStore8 (memory state) (fromIntegral p) (fromIntegral $ val .&. 0xFF)
  return $
    state { stack=rest }

runOperation _ SLOAD _ state@VMState{stack=(p:rest)} = do
  let val = fromMaybe 0 $ M.lookup p (storage state)
  return $ state { stack=val:rest }
  
runOperation _ SSTORE _ state@VMState{stack=(p:val:rest)} = do
  let oldVal = fromMaybe 0 $ M.lookup val (storage state)
  let extraGasUsed =
        case (oldVal, val) of
          (0, x) | x /= 0 -> 100
          (x, 0) | x /= 0 -> -100
          _ -> 0
  return $ state { stack=rest, storage=M.insert p val $ storage state, vmGasRemaining = vmGasRemaining state - extraGasUsed }

runOperation _ JUMP _ state@VMState{stack=(p:rest)} =
  return $ state { stack=rest, pc=fromIntegral p }

runOperation _ JUMPI _ state@VMState{stack=(p:cond:rest)} =
  return $ state { stack=rest, pc=if word2562Bool cond then fromIntegral p else (pc state) }

runOperation _ PC _ state =
  return state{stack=fromIntegral (pc state):stack state}

runOperation _ MSIZE _ state@VMState{memory=Memory mSize _} =
  return state{stack=mSize:stack state}

runOperation _ GAS _ state =
  return $ state { stack=fromInteger (vmGasRemaining state):stack state }

runOperation _ (PUSH vals) _ state =
  return $
  state { stack=(fromIntegral <$> vals) ++ stack state }



--               | CREATE | CALL | RETURN | SUICIDE deriving (Show, Eq, Ord)



runOperation _ RETURN _ state =
  return $ state { done=True }

runOperation _ SUICIDE _ state =
  return $ state { done=True, markedForSuicide=True }

runOperation _ x _ _ = error $ "Missing case in runOperation: " ++ show x



-------------------




movePC::VMState->Int->VMState
movePC state l = state{ pc=pc state + l }

opGasPrice::Operation->Integer
opGasPrice STOP = 0
opGasPrice MSTORE = 2
opGasPrice SSTORE = 100
opGasPrice _ = 1

decreaseGas::Operation->VMState->IO VMState
decreaseGas op state = do
  let val = opGasPrice op
  if val <= vmGasRemaining state
    then return (state{ vmGasRemaining = vmGasRemaining state - opGasPrice op })
    else return (state{ vmGasRemaining = 0,
                        vmException = Just OutOfGasException })

runCode::DB->Environment->VMState->IO VMState
runCode db env state = do
  let (op, len) = getOperationAt (envCode env) (pc state)
  state' <- decreaseGas op state
  result <- liftIO $ runOperation db op env state'
  case result of
    VMState{vmException=Just _} -> return result
    VMState{done=True} -> return $ movePC result len
    state2 -> runCode db env $ movePC state2 len

runCodeFromStart::DB->Integer->Environment->IO VMState
runCodeFromStart db gasLimit env = do
  vmState <- liftIO startingState
  runCode db env vmState{vmGasRemaining=gasLimit}

