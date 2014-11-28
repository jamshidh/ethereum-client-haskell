
module VM (
  runCodeFromStart
  ) where

import Prelude hiding (LT, GT, EQ)

import Control.Monad.IO.Class
import Data.Bits
import qualified Data.ByteString as B
import Data.Time.Clock.POSIX
import Network.Haskoin.Crypto (Word256)

import Context
import Data.Address
import Data.AddressState
import Data.Block
import Database.MerklePatricia
import qualified Data.NibbleString as N
import Data.RLP
import ExtDBs
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

binaryAction::(Word256->Word256->Word256)->Environment->VMState->ContextM VMState
binaryAction action _ state@VMState{stack=x:y:rest} = return state{stack=x `action` y:rest}
binaryAction _ env state = liftIO $ addErr "stack did not contain enough elements" (envCode env) state

unaryAction::(Word256->Word256)->Environment->VMState->ContextM VMState
unaryAction action _ state@VMState{stack=x:rest} = return state{stack=action x:rest}
unaryAction _ env state = liftIO $ addErr "stack did not contain enough elements" (envCode env) state



runOperation::Operation->Environment->VMState->ContextM VMState
runOperation STOP _ state = return state{done=True}

runOperation ADD env state = binaryAction (+) env state
runOperation MUL env state = binaryAction (*) env state
runOperation SUB env state = binaryAction (-) env state
runOperation DIV env state = binaryAction quot env state
runOperation SDIV env state = binaryAction undefined env state
runOperation MOD env state = binaryAction mod env state
runOperation SMOD env state = binaryAction undefined env state
runOperation EXP env state = binaryAction (^) env state
runOperation NEG env state = unaryAction negate env state
runOperation LT env state = binaryAction ((bool2Word256 .) . (<)) env state
runOperation GT env state = binaryAction ((bool2Word256 .) . (>)) env state
runOperation SLT env state = binaryAction undefined env state
runOperation SGT env state = binaryAction undefined env state
runOperation EQ env state = binaryAction ((bool2Word256 .) . (==)) env state
runOperation NOT env state = unaryAction (bool2Word256 . not . word2562Bool) env state
runOperation AND env state = binaryAction (.&.) env state
runOperation OR env state = binaryAction (.|.) env state
runOperation XOR env state = binaryAction xor env state

runOperation BYTE env state = binaryAction (\x y -> y `shiftR` fromIntegral x .&. 0xFF) env state

runOperation SHA3 _ state@VMState{stack=(p:size:rest)} = do
  SHA theHash <- fmap hash $ liftIO $ mLoadByteString (memory state) p size
  return state{stack=theHash:rest}

runOperation ADDRESS Environment{envOwner=Address a} state = return state{stack=fromIntegral a:stack state}

runOperation BALANCE _ state@VMState{stack=(x:rest)} = do
  addressState <- getAddressState (Address $ fromIntegral x)
  return state{stack=fromIntegral (balance addressState):rest}
runOperation BALANCE env state = liftIO $ addErr "stack did not contain enough elements" (envCode env) state

runOperation ORIGIN Environment{envSender=Address sender} state = return state{stack=fromIntegral sender:stack state}

runOperation CALLER Environment{envOrigin=Address owner} state = return state{stack=fromIntegral owner:stack state}

runOperation CALLVALUE Environment{envValue=val} state = return state{stack=fromIntegral val:stack state}

runOperation CALLDATALOAD Environment{envInputData=d} state@VMState{stack=p:rest} = do
  let val = bytes2Integer $ B.unpack $ B.take 32 $ B.drop (fromIntegral p) d
  return state{stack=fromIntegral val:rest}
runOperation CALLDATALOAD _ s = return s{ vmException=Just StackTooSmallException } 

runOperation CALLDATASIZE Environment{envInputData=d} state = return state{stack=fromIntegral (B.length d):stack state}

runOperation CALLDATACOPY Environment{envInputData=d} state@VMState{stack=memP:codeP:size:rest} = do
  liftIO $ mStoreByteString (memory state) memP $ B.take (fromIntegral size) $ B.drop (fromIntegral codeP) d
  return state{stack=rest}

runOperation CODESIZE Environment{envCode=c} state = return state{stack=fromIntegral (codeLength c):stack state}

runOperation CODECOPY Environment{envCode=Code c} state@VMState{stack=memP:codeP:size:rest} = do
  beforeMemSize <- liftIO $ getSize $ memory state
  liftIO $ mStoreByteString (memory state) memP $ B.take (fromIntegral size) $ B.drop (fromIntegral codeP) c
  afterMemory <- liftIO $ getSize (memory state)
  let extraMemory = afterMemory - beforeMemSize 
  liftIO $ putStrLn $ "before: " ++ show beforeMemSize
  liftIO $ putStrLn $ "after: " ++ show afterMemory
  liftIO $ putStrLn $ "extra: " ++ show extraMemory
  return state{stack=rest} -- temporarily moved     , vmGasRemaining = vmGasRemaining state - fromIntegral extraMemory}

runOperation GASPRICE Environment{envGasPrice=gp} state = return state{stack=fromIntegral gp:stack state}

runOperation PREVHASH Environment{envBlock=Block{blockData=BlockData{parentHash=SHA prevHash}}} state = return state{stack=prevHash:stack state}

runOperation COINBASE Environment{envBlock=Block{blockData=BlockData{coinbase=Address cb}}} state = return state{stack=fromIntegral cb:stack state}

runOperation TIMESTAMP Environment{envBlock=Block{blockData=bd}} state = return state{stack=round (utcTimeToPOSIXSeconds $ timestamp bd):stack state}
runOperation NUMBER Environment{envBlock=Block{blockData=bd}} state = return state{stack=fromIntegral (number bd):stack state}
runOperation DIFFICULTY Environment{envBlock=Block{blockData=bd}} state = return state{stack=fromIntegral (difficulty bd):stack state}
runOperation GASLIMIT Environment{envBlock=Block{blockData=bd}} state = return state{stack=fromIntegral (gasLimit bd):stack state}

runOperation POP _ state@VMState{stack=_:rest} = return state{stack=rest}
runOperation POP env state = liftIO $ addErr "Stack did not contain any items" (envCode env) state

runOperation DUP _ state@VMState{stack=x:rest} = return state{stack=x:x:rest}
runOperation DUP env state = liftIO $ addErr "Stack did not contain any items" (envCode env) state

runOperation SWAP _ state@VMState{stack=x:y:rest} = return state{stack=y:x:rest}
runOperation SWAP env state = liftIO $ addErr "Stack did not contain enough items" (envCode env) state

runOperation MLOAD _ state@VMState{stack=(p:rest)} = do
  bytes <- liftIO $ mLoad (memory state) p --sequence $ readArray (memory state) <$> fromIntegral <$> [p..p+31]
  return $ state { stack=fromInteger (bytes2Integer bytes):rest }
  
runOperation MSTORE _ state@VMState{stack=(p:val:rest)} = do
  liftIO $ mStore (memory state) p val
  return state{stack=rest}

runOperation MSTORE8 _ state@VMState{stack=(p:val:rest)} = do
  liftIO $ mStore8 (memory state) (fromIntegral p) (fromIntegral $ val .&. 0xFF)
  return $
    state { stack=rest }

runOperation SLOAD _ state@VMState{stack=(p:rest)} = do
  vals <- getStorageKeyVals (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
  let val = case vals of
              [] -> 0
              [x] -> fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode $ snd x
              _ -> error "Multiple values in storage"

  return $ state { stack=val:rest }
  
runOperation SSTORE _ state@VMState{stack=(p:val:rest)} = do
  if val == 0
    then deleteStorageKey (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
    else putStorageKeyVal (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p) (rlpEncode $ rlpSerialize $ rlpEncode $ toInteger val)
  return $ state { stack=rest }

runOperation JUMP _ state@VMState{stack=(p:rest)} =
  return $ state { stack=rest, pc=fromIntegral p }

runOperation JUMPI _ state@VMState{stack=(p:cond:rest)} =
  return $ state { stack=rest, pc=if word2562Bool cond then fromIntegral p else pc state }

runOperation PC _ state =
  return state{stack=fromIntegral (pc state):stack state}

runOperation MSIZE _ state@VMState{memory=m} = do
  memSize <- liftIO $ getSize m
  return state{stack=memSize:stack state}

runOperation GAS _ state =
  return $ state { stack=fromInteger (vmGasRemaining state):stack state }

runOperation (PUSH vals) _ state =
  return $
  state { stack=fromIntegral (bytes2Integer vals):stack state }



--               | CREATE | CALL | RETURN | SUICIDE deriving (Show, Eq, Ord)



runOperation RETURN _ state@VMState{stack=[address, size]} = do
  retVal <- liftIO $ mLoadByteString (memory state) address size
  return $ state { done=True, returnVal=Just retVal }

runOperation RETURN _ VMState{stack=x} | length x > 2 =
  error "Stack was too large in when RETURN was called"

runOperation RETURN _ state =
  return $ state { vmException=Just StackTooSmallException } 

runOperation SUICIDE _ state =
  return $ state { done=True, markedForSuicide=True }

runOperation x _ _ = error $ "Missing case in runOperation: " ++ show x



-------------------




movePC::VMState->Int->VMState
movePC state l = state{ pc=pc state + l }

opGasPrice::VMState->Operation->ContextM Integer
opGasPrice _ STOP = return 0
--opGasPrice _ MSTORE = return 2
opGasPrice VMState{ stack=_:_:_ } SLOAD = return 20
opGasPrice VMState{ stack=p:val:_ } SSTORE = do
  oldVals <- getStorageKeyVals (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
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
opGasPrice _ _ = return 1

decreaseGas::Operation->VMState->ContextM VMState
decreaseGas op state = do
  val <- opGasPrice state op
  if val <= vmGasRemaining state
    then return (state{ vmGasRemaining = vmGasRemaining state - val })
    else return (state{ vmGasRemaining = 0,
                        vmException = Just OutOfGasException })

runCode::Environment->VMState->ContextM VMState
runCode env state = do
  let (op, len) = getOperationAt (envCode env) (pc state)
  state' <- decreaseGas op state
  result <- runOperation op env state'
  case result of
    VMState{vmException=Just _} -> return result{ vmGasRemaining = 0 } 
    VMState{done=True} -> do
                         memSize <- liftIO $ getSize $ memory result
                         liftIO $ putStrLn ("memSize : " ++ show memSize)
                         return $ movePC result{ vmGasRemaining = vmGasRemaining result - fromIntegral memSize } len
    state2 -> runCode env $ movePC state2 len

runCodeFromStart::SHAPtr->Integer->Environment->ContextM VMState
runCodeFromStart storageRoot' gasLimit' env = do
  setStorageStateRoot storageRoot'
  vmState <- liftIO startingState
  runCode env vmState{vmGasRemaining=gasLimit'}

