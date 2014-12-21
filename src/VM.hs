{-# LANGUAGE OverloadedStrings #-}

module VM (
  runCodeFromStart
  ) where

import Prelude hiding (LT, GT, EQ)

import Control.Monad.IO.Class
import Data.Bits
import qualified Data.ByteString as B
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Time.Clock.POSIX
import Network.Haskoin.Crypto (Word256)
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Context
import qualified Colors as CL
import Data.Address
import Data.AddressState
import Data.Block
import qualified Data.NibbleString as N
import Data.RLP
import DB.CodeDB
import DB.ModifyStateDB
import ExtDBs
import Format
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
binaryAction _ _ state = return state{vmException=Just StackTooSmallException}

unaryAction::(Word256->Word256)->Environment->VMState->ContextM VMState
unaryAction action _ state@VMState{stack=x:rest} = return state{stack=action x:rest}
unaryAction _ _ state = return state{vmException=Just StackTooSmallException}


s256ToInteger::Word256->Integer
s256ToInteger i | i < 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF = toInteger i
s256ToInteger i = 0x10000000000000000000000000000000000000000000000000000000000000000 - toInteger i

runOperation::Operation->Environment->VMState->ContextM VMState
runOperation STOP _ state = return state{done=True}

runOperation ADD env state = binaryAction (+) env state
runOperation MUL env state = binaryAction (*) env state
runOperation SUB env state = binaryAction (-) env state
runOperation DIV env state = binaryAction quot env state
runOperation SDIV env state = binaryAction ((fromIntegral .) . quot `on` s256ToInteger) env state
runOperation MOD env state = binaryAction mod env state
runOperation SMOD env state = binaryAction ((fromIntegral .) . mod `on` s256ToInteger) env state
runOperation ADDMOD env state = binaryAction ((fromIntegral .) . (+) `on` s256ToInteger) env state
runOperation EXP env state = binaryAction (^) env state
runOperation NEG env state = unaryAction negate env state
runOperation LT env state = binaryAction ((bool2Word256 .) . (<)) env state
runOperation GT env state = binaryAction ((bool2Word256 .) . (>)) env state
runOperation SLT env state = binaryAction ((bool2Word256 .) . ((<) `on` s256ToInteger)) env state
runOperation SGT env state = binaryAction ((bool2Word256 .) . ((>) `on` s256ToInteger)) env state
runOperation EQ env state = binaryAction ((bool2Word256 .) . (==)) env state
runOperation ISZERO env state = unaryAction (bool2Word256 . not . word2562Bool) env state
runOperation AND env state = binaryAction (.&.) env state
runOperation OR env state = binaryAction (.|.) env state
runOperation XOR env state = binaryAction xor env state

runOperation BYTE env state = binaryAction (\x y -> y `shiftR` (8*(31 - fromIntegral x)) .&. 0xFF) env state

runOperation SHA3 _ state@VMState{stack=(p:size:rest)} = do
  SHA theHash <- fmap hash $ liftIO $ mLoadByteString state p size
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
  let val = bytes2Integer $ appendZerosTo32 $ B.unpack $ B.take 32 $ B.drop (fromIntegral p) d
  return state{stack=fromIntegral val:rest}
    where
      appendZerosTo32 x | length x < 32 = x ++ replicate (32-length x) 0
      appendZerosTo32 x = x
      
runOperation CALLDATALOAD _ s = return s{ vmException=Just StackTooSmallException } 

runOperation CALLDATASIZE Environment{envInputData=d} state = return state{stack=fromIntegral (B.length d):stack state}

runOperation CALLDATACOPY Environment{envInputData=d} state@VMState{stack=memP:codeP:size:rest} = do
  state'<-liftIO $ mStoreByteString state memP $ B.take (fromIntegral size) $ B.drop (fromIntegral codeP) d
  return state'{stack=rest}

runOperation CODESIZE Environment{envCode=c} state = return state{stack=fromIntegral (codeLength c):stack state}

runOperation CODECOPY Environment{envCode=Code c} state@VMState{stack=memP:codeP:size:rest} = do
  state' <- liftIO $ mStoreByteString state memP $ B.take (fromIntegral size) $ B.drop (fromIntegral codeP) c
  return state'{stack=rest}

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
  bytes <- liftIO $ mLoad state p
  return $ state { stack=fromInteger (bytes2Integer bytes):rest }
  
runOperation MSTORE _ state@VMState{stack=(p:val:rest)} = do
  state' <- liftIO $ mStore state p val
  return state'{stack=rest}

runOperation MSTORE8 _ state@VMState{stack=(p:val:rest)} = do
  state' <- liftIO $ mStore8 state (fromIntegral p) (fromIntegral $ val .&. 0xFF)
  return state'{stack=rest}

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
runOperation SSTORE _ state =
  return $ state { vmException=Just StackTooSmallException } 

--TODO- refactor so that I don't have to use this -1 hack
runOperation JUMP _ state@VMState{stack=(p:rest)} =
  return $ state { stack=rest, pc=fromIntegral p - 1} -- Subtracting 1 to compensate for the pc-increment that occurs every step.

runOperation JUMPI _ state@VMState{stack=(p:cond:rest)} =
  return $ state { stack=rest, pc=if word2562Bool cond then fromIntegral p - 1 else pc state }

runOperation PC _ state =
  return state{stack=fromIntegral (pc state):stack state}

runOperation MSIZE _ state@VMState{memory=m} = do
  memSize <- liftIO $ getSize m
  return state{stack=memSize:stack state}

runOperation GAS _ state =
  return $ state { stack=fromInteger (vmGasRemaining state):stack state }

runOperation JUMPDEST _ state = return state

runOperation (PUSH vals) _ state =
  return $
  state { stack=fromIntegral (bytes2Integer vals):stack state }



--               | CREATE | SUICIDE deriving (Show, Eq, Ord)

runOperation DUP1 _ state@VMState{stack=s@(v:_)} = return state{stack=v:s}
runOperation DUP2 _ state@VMState{stack=s@(_:v:_)} = return state{stack=v:s}
runOperation DUP3 _ state@VMState{stack=s@(_:_:v:_)} = return state{stack=v:s}
runOperation DUP4 _ state@VMState{stack=s@(_:_:_:v:_)} = return state{stack=v:s}
runOperation DUP5 _ state@VMState{stack=s@(_:_:_:_:v:_)} = return state{stack=v:s}
runOperation DUP6 _ state@VMState{stack=s@(_:_:_:_:_:v:_)} = return state{stack=v:s}
runOperation DUP7 _ state@VMState{stack=s@(_:_:_:_:_:_:v:_)} = return state{stack=v:s}
runOperation DUP8 _ state@VMState{stack=s@(_:_:_:_:_:_:_:v:_)} = return state{stack=v:s}
runOperation DUP9 _ state@VMState{stack=s@(_:_:_:_:_:_:_:_:v:_)} = return state{stack=v:s}
runOperation DUP10 _ state@VMState{stack=s@(_:_:_:_:_:_:_:_:_:v:_)} = return state{stack=v:s}
runOperation DUP11 _ state@VMState{stack=s@(_:_:_:_:_:_:_:_:_:_:v:_)} = return state{stack=v:s}
runOperation DUP12 _ state@VMState{stack=s@(_:_:_:_:_:_:_:_:_:_:_:v:_)} = return state{stack=v:s}
runOperation DUP13 _ state@VMState{stack=s@(_:_:_:_:_:_:_:_:_:_:_:_:v:_)} = return state{stack=v:s}
runOperation DUP14 _ state@VMState{stack=s@(_:_:_:_:_:_:_:_:_:_:_:_:_:v:_)} = return state{stack=v:s}
runOperation DUP15 _ state@VMState{stack=s@(_:_:_:_:_:_:_:_:_:_:_:_:_:_:v:_)} = return state{stack=v:s}
runOperation DUP16 _ state@VMState{stack=s@(_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:v:_)} = return state{stack=v:s}





runOperation CALL env state@VMState{stack=(gas:to:value:inOffset:inSize:outOffset:outSize:rest)} = do

  inputData <- liftIO $ mLoadByteString state inOffset inSize

  let address = Address $ fromIntegral to

  addressState <- getAddressState address
  code <- 
      fromMaybe B.empty <$>
                getCode (codeHash addressState)


  (nestedState, _) <-
    runCodeFromStart address
     (fromIntegral gas)
     Environment {
       envOwner = error "envOwner undefined",
       envOrigin = error "envOrigin undefined",
       envGasPrice = error "envGasPrice undefined",
       envInputData = inputData,
       envSender = error "envSender undefined",
       envValue = fromIntegral value,
       envCode = Code code,
       envBlock = error "envBlock undefined"
       }

  let retVal = fromMaybe B.empty $ returnVal nestedState
 
  state' <- liftIO $ mStoreByteString state outOffset retVal

  let success = 1

  pay (envOwner env) address (fromIntegral value)

  return state'{stack=success:rest}

runOperation CALLCODE env state@VMState{stack=gas:to:value:inOffset:inSize:outOffset:outSize:rest} = do

  inputData <- liftIO $ mLoadByteString state inOffset inSize
  let address = Address $ fromIntegral to
  addressState <- getAddressState address
  code <- 
      fromMaybe B.empty <$>
                getCode (codeHash addressState)

  (nestedState, _) <-
    runCodeFromStart (envOwner env)
     (fromIntegral gas)
     Environment {
       envOwner = error "envOwner undefined",
       envOrigin = error "envOrigin undefined",
       envGasPrice = error "envGasPrice undefined",
       envInputData = inputData,
       envSender = error "envSender undefined",
       envValue = fromIntegral value,
       envCode = Code code,
       envBlock = error "envBlock undefined"
       }

  let retVal = fromMaybe B.empty $ returnVal nestedState
 
  state' <- liftIO $ mStoreByteString state outOffset retVal

  let success = 1

  pay (envOwner env) address (fromIntegral value)

  return state'{stack=success:rest}



runOperation CALLCODE _ state =
  return $ state { vmException=Just StackTooSmallException } 

                                                               

runOperation RETURN _ state@VMState{stack=(address:size:rest)} = do
  retVal <- liftIO $ mLoadByteString state address size
  return $ state { stack=rest, done=True, returnVal=Just retVal }

runOperation RETURN _ state =
  return $ state { vmException=Just StackTooSmallException } 

runOperation SUICIDE _ state =
  return $ state { done=True, markedForSuicide=True }


runOperation MalformedOpcode _ state = do
  liftIO $ putStrLn $ CL.red "Malformed Opcode"
  return state { vmException=Just MalformedOpcodeException }

runOperation x _ _ = error $ "Missing case in runOperation: " ++ show x



-------------------




movePC::VMState->Int->VMState
movePC state l = state{ pc=pc state + l }

opGasPrice::VMState->Operation->ContextM Integer
opGasPrice _ STOP = return 0
opGasPrice _ SUICIDE = return 0


opGasPrice _ BALANCE = return 20
opGasPrice _ SHA3 = return 20
opGasPrice _ SLOAD = return 20
opGasPrice _ CALL = return 20

opGasPrice _  CREATE = return 100

opGasPrice VMState{stack=_:_:size:_} CODECOPY = return $ 1 + ceiling (fromIntegral size / (32::Double))
opGasPrice VMState{stack=_:_:size:_} CALLDATACOPY = return $ 1 + ceiling (fromIntegral size / (32::Double))
opGasPrice VMState{ stack=p:val:_ } SSTORE = do
  oldVals <- getStorageKeyVals (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
  let oldVal =
          case oldVals of
            [] -> 0::Word256
            [x] -> fromInteger $ rlpDecode $ snd x
            _ -> error "multiple values in storage"
  return $
    case (oldVal, val) of
      (0, x) | x /= 0 -> 300
      (x, 0) | x /= 0 -> 0
      _ -> 100
opGasPrice _ _ = return 1

--missing stuff
--Gexp 1 Partial payment for an EXP operation.
--Gexpbyte 1 Partial payment when multiplied by dlog256(exponent)e for the EXP operation.
--Glog 1 Partial payment for a LOG operation.
--Glogdata 1 Paid for each byte in a LOG operation’s data.
--Glogtopic 1 Paid for each topic of a LOG operation.









decreaseGas::Integer->VMState->VMState
decreaseGas val state = do
  if val <= vmGasRemaining state
    then state{ vmGasRemaining = vmGasRemaining state - val }
    else state{ vmGasRemaining = 0, vmException = Just OutOfGasException }

decreaseGasForOp::Operation->VMState->ContextM VMState
decreaseGasForOp op state = do
  val <- opGasPrice state op
  return $ decreaseGas val state

nibbleString2ByteString::N.NibbleString->B.ByteString
nibbleString2ByteString (N.EvenNibbleString s) = s
nibbleString2ByteString (N.OddNibbleString c s) = c `B.cons` s


runCode::Environment->VMState->Int->ContextM VMState
runCode env state c = do
  let (op, len) = getOperationAt (envCode env) (pc state)
  liftIO $ putStrLn $ " > OP: " ++ show op ++ " #" ++ show c ++ " (" ++ show (vmGasRemaining state) ++ ")"
  state' <- decreaseGasForOp op state
  result <- runOperation op env state'
  memString <- liftIO $ getShow (memory result)
  --liftIO $ putStrLn $ " > memory: " ++ memString
  liftIO $ putStrLn "STACK"
  liftIO $ putStrLn $ unlines (("    " ++) <$> padZeros 64 <$> flip showHex "" <$> stack result)
  --kvs <- getStorageKeyVals ""
  --liftIO $ putStrLn $ unlines (map (\(k, v) -> "0x" ++ showHex (byteString2Integer $ nibbleString2ByteString k) "" ++ ": 0x" ++ showHex (rlpDecode $ rlpDeserialize $ rlpDecode v::Integer) "") kvs)
  case result of
    VMState{vmException=Just _} -> return result{ vmGasRemaining = 0 } 
    VMState{done=True} -> return $ movePC result len
    state2 -> runCode env (movePC state2 len) (c+1)

runCodeFromStart::Address->Integer->Environment->ContextM (VMState, SHAPtr)
runCodeFromStart address gasLimit' env = do
  addressState <- getAddressState address
  --liftIO $ putStrLn $ "Running code:\n    Input Data = " ++ format (envInputData env)
  oldStateRoot <- getStorageStateRoot
  setStorageStateRoot (contractRoot addressState)

  vmState <- liftIO startingState
  state' <- runCode env{envOwner=address} vmState{vmGasRemaining=gasLimit'} 0

  newStateRoot <- getStorageStateRoot
  --newAddressState <- getAddressState address
  --putAddressState address newAddressState{contractRoot=newStateRoot} 
  setStorageStateRoot oldStateRoot

  return (state', newStateRoot)
