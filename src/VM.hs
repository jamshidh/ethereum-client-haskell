
module VM (
  runCodeFromStart
  ) where

import Prelude hiding (LT, GT, EQ)

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.ByteString as B
import Data.Functor
import Data.Maybe
import Data.Time.Clock.POSIX
import Network.Haskoin.Crypto (Word256)

import Data.Address
import Data.AddressState
import Data.Block
import DB.DBs
import DB.EthDB
import qualified Data.NibbleString as N
import Data.RLP
import SHA
import Util
import VM.Code
import VM.Environment
import VM.Memory
import VM.Opcodes
import VM.VMState

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

runOperation _ CALLER Environment{envOrigin=Address owner} state = return state{stack=fromIntegral owner:stack state}

runOperation _ CALLVALUE Environment{envValue=val} state = return state{stack=fromIntegral val:stack state}

runOperation _ CALLDATALOAD Environment{envInputData=d} state@VMState{stack=p:rest} = do
  let val = bytes2Integer $ B.unpack $ B.take 32 $ B.drop (fromIntegral p) d
  return state{stack=fromIntegral val:rest}
runOperation _ CALLDATALOAD _ s = return s{ vmException=Just StackTooSmallException } 

runOperation _ CALLDATASIZE Environment{envInputData=d} state = return state{stack=fromIntegral (B.length d):stack state}

runOperation _ CALLDATACOPY Environment{envInputData=d} state@VMState{stack=memP:codeP:size:rest} = do
  mStoreByteString (memory state) memP $ B.take (fromIntegral size) $ B.drop (fromIntegral codeP) d
  return state{stack=rest}

runOperation _ CODESIZE Environment{envCode=c} state = return state{stack=fromIntegral (codeLength c):stack state}

runOperation _ CODECOPY Environment{envCode=Code c} state@VMState{stack=memP:codeP:size:rest} = do
  beforeMemSize <- getSize $ memory state
  mStoreByteString (memory state) memP $ B.take (fromIntegral size) $ B.drop (fromIntegral codeP) c
  afterMemory <- getSize (memory state)
  let extraMemory = afterMemory - beforeMemSize 
  liftIO $ putStrLn $ "before: " ++ show beforeMemSize
  liftIO $ putStrLn $ "after: " ++ show afterMemory
  liftIO $ putStrLn $ "extra: " ++ show extraMemory
  return state{stack=rest} -- temporarily moved     , vmGasRemaining = vmGasRemaining state - fromIntegral extraMemory}

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

runOperation db SLOAD _ state@VMState{stack=(p:rest)} = do
  vals <-  runResourceT $ getKeyVals db{stateRoot=storageRoot state} (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
  let val = case vals of
              [] -> 0
              [x] -> fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode $ snd x
              _ -> error "Multiple values in storage"

  return $ state { stack=val:rest }
  
runOperation db SSTORE _ state@VMState{stack=(p:val:rest)} = do
  db' <- runResourceT $ putKeyVal db{stateRoot=storageRoot state} (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p) (rlpEncode $ rlpSerialize $ rlpEncode $ toInteger val)
  return $ state { stack=rest, storageRoot=stateRoot db' }

runOperation _ JUMP _ state@VMState{stack=(p:rest)} =
  return $ state { stack=rest, pc=fromIntegral p }

runOperation _ JUMPI _ state@VMState{stack=(p:cond:rest)} =
  return $ state { stack=rest, pc=if word2562Bool cond then fromIntegral p else (pc state) }

runOperation _ PC _ state =
  return state{stack=fromIntegral (pc state):stack state}

runOperation _ MSIZE _ state@VMState{memory=m} = do
  memSize <- getSize m
  return state{stack=memSize:stack state}

runOperation _ GAS _ state =
  return $ state { stack=fromInteger (vmGasRemaining state):stack state }

runOperation _ (PUSH vals) _ state =
  return $
  state { stack=fromIntegral (bytes2Integer vals):stack state }



--               | CREATE | CALL | RETURN | SUICIDE deriving (Show, Eq, Ord)



runOperation _ RETURN _ state@VMState{stack=[address, size]} = do
  retVal <- liftIO $ mLoadByteString (memory state) address size
  return $ state { done=True, returnVal=Just retVal }

runOperation _ RETURN _ VMState{stack=x} | length x > 2 =
  error "Stack was too large in when RETURN was called"

runOperation _ RETURN _ state = do
  return $ state { vmException=Just StackTooSmallException } 

runOperation _ SUICIDE _ state =
  return $ state { done=True, markedForSuicide=True }

runOperation _ x _ _ = error $ "Missing case in runOperation: " ++ show x



-------------------




movePC::VMState->Int->VMState
movePC state l = state{ pc=pc state + l }

opGasPrice::DB->VMState->Operation->IO Integer
opGasPrice _ _ STOP = return 0
opGasPrice _ _ MSTORE = return 2
opGasPrice _ VMState{ stack=_:_:_ } SLOAD = return 20
opGasPrice db state@VMState{ stack=p:val:_ } SSTORE = do
  oldVals <- runResourceT $ getKeyVals db{stateRoot=storageRoot state} (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
  let oldVal =
          case oldVals of
            [] -> 0::Word256
            [x] -> fromInteger $ rlpDecode $ snd x
            _ -> error "multiple values in storage"
  return $
    case (oldVal, val) of
      (0, x) | x /= 0 -> 200
      (x, 0) | x /= 0 -> 0
      _ -> 100
opGasPrice _ _ _ = return 1

decreaseGas::DB->Operation->VMState->IO VMState
decreaseGas db op state = do
  val <- opGasPrice db state op
  if val <= vmGasRemaining state
    then return (state{ vmGasRemaining = vmGasRemaining state - val })
    else return (state{ vmGasRemaining = 0,
                        vmException = Just OutOfGasException })

runCode::DB->Environment->VMState->IO VMState
runCode db env state = do
  let (op, len) = getOperationAt (envCode env) (pc state)
  state' <- decreaseGas db op state
  result <- runOperation db op env state'
  case result of
    VMState{vmException=Just _} -> return result{ vmGasRemaining = 0 } 
    VMState{done=True} -> do
                         memSize <- getSize $ memory result
                         putStrLn ("memSize : " ++ show memSize)
                         return $ movePC result{ vmGasRemaining = vmGasRemaining result - fromIntegral memSize } len
    state2 -> runCode db env $ movePC state2 len

runCodeFromStart::DB->SHAPtr->Integer->Environment->IO VMState
runCodeFromStart db storageRoot' gasLimit' env = do
  vmState <- liftIO $ startingState storageRoot'
  runCode db env vmState{vmGasRemaining=gasLimit'}

