{-# LANGUAGE OverloadedStrings #-}

module Blockchain.VM (
  runCodeFromStart,
  runCodeForTransaction',
  create
  ) where

import Prelude hiding (LT, GT, EQ)

import Control.Monad.IO.Class
import Control.Monad.State hiding (state)
import Data.Bits
import qualified Data.ByteString as B
import Data.Char
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Time.Clock.POSIX
import Network.Haskoin.Crypto (Word256)
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Blockchain.Colors as CL
import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressState
import Blockchain.Data.Block
import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.DB.CodeDB
import Blockchain.DB.ModifyStateDB
import Blockchain.ExtDBs
import Blockchain.Format
import Blockchain.SHA
import Blockchain.Util
import Blockchain.VM.Code
import Blockchain.VM.Environment
import Blockchain.VM.Memory
import Blockchain.VM.Opcodes
import Blockchain.VM.VMState
import qualified Data.NibbleString as N


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


swapn::Int->VMState->ContextM VMState
swapn n state@VMState{stack=v1:rest1} | length rest1 >= n = return state{stack=v2:(middle++(v1:rest2))}
    where
      (middle, v2:rest2) = splitAt (n-1) rest1
swapn _ state = return state{vmException=Just StackTooSmallException}


--TODO- This really should be in its own monad!
--The monad should manage everything in the VM and environment (extending the ContextM), and have pop and push operations, perhaps even automating pc incrementing, gas charges, etc.
--The code would simplify greatly, but I don't feel motivated to make the change now since things work.

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
runOperation ISZERO env state = unaryAction (bool2Word256 . (==0)) env state
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

runOperation CALLER Environment{envSender=Address owner} state = return state{stack=fromIntegral owner:stack state}

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

runOperation LOG0 _ state@VMState{stack=_:_:rest} = return state{stack=rest}
runOperation LOG1 _ state@VMState{stack=_:_:_:rest} = return state{stack=rest}
runOperation LOG2 _ state@VMState{stack=_:_:_:_:rest} = return state{stack=rest}
runOperation LOG3 _ state@VMState{stack=_:_:_:_:_:rest} = return state{stack=rest}
runOperation LOG4 _ state@VMState{stack=_:_:_:_:_:_:rest} = return state{stack=rest}

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
  memSize <- liftIO $ getSizeInBytes m
  return state{stack=memSize:stack state}

runOperation GAS _ state =
  return $ state { stack=fromInteger (vmGasRemaining state):stack state }

runOperation JUMPDEST _ state = return state

runOperation (PUSH vals) _ state =
  return $
  state { stack=fromIntegral (bytes2Integer vals):stack state }



--               | SUICIDE deriving (Show, Eq, Ord)

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

runOperation SWAP1 _ state = swapn 1 state
runOperation SWAP2 _ state = swapn 2 state
runOperation SWAP3 _ state = swapn 3 state
runOperation SWAP4 _ state = swapn 4 state
runOperation SWAP5 _ state = swapn 5 state
runOperation SWAP6 _ state = swapn 6 state
runOperation SWAP7 _ state = swapn 7 state
runOperation SWAP8 _ state = swapn 8 state
runOperation SWAP9 _ state = swapn 9 state
runOperation SWAP10 _ state = swapn 10 state
runOperation SWAP11 _ state = swapn 11 state
runOperation SWAP12 _ state = swapn 12 state
runOperation SWAP13 _ state = swapn 13 state
runOperation SWAP14 _ state = swapn 14 state
runOperation SWAP15 _ state = swapn 15 state
runOperation SWAP16 _ state = swapn 16 state



runOperation CREATE env state@VMState{stack=value:input:size:rest} = do
  addressState <- getAddressState $ envOwner env
  let newAddress = getNewAddress (envOwner env) (addressStateNonce addressState)
  init <- liftIO (Code <$> mLoadByteString state input size)

  resultRemainingGas <- create (envBlock env) (callDepth state + 1) (envSender env) (toInteger value) (envGasPrice env) (vmGasRemaining state) newAddress init

  let Address result = newAddress --TODO- check for failure, set result to 0 if failed

  return state{stack=fromIntegral result:rest, vmGasRemaining=resultRemainingGas}

{-

    putAddressState (envOwner env) addressState{addressStateNonce=addressStateNonce addressState + 1}


    liftIO $ putStrLn $ "qqqqqqqqqqqqqqqqqqq:" ++ show (B.length codeBytes)

    addCode codeBytes

    putAddressState newAddress 
                    blankAddressState
                    {
                      codeHash=hash codeBytes
                    }

    (state', retValue) <- nestedRun env state{stack=rest} (fromIntegral $ vmGasRemaining state) newAddress value B.empty

    case retValue of
      Just bytes -> do
                    addCode bytes
                    addressState' <- getAddressState newAddress
                    putAddressState newAddress addressState'{codeHash=hash bytes}
      _ -> return ()

    let usedGas = vmGasRemaining state - vmGasRemaining state'

    return state'{vmGasRemaining = vmGasRemaining state' - usedGas}

-}

runOperation CALL env state@VMState{stack=(gas:to:value:inOffset:inSize:outOffset:_:rest)} = do

  inputData <- liftIO $ mLoadByteString state inOffset inSize

  (state', retValue) <- nestedRun env state{stack=rest} gas (Address $ fromIntegral to) value inputData

  case retValue of
    Just bytes -> liftIO $ mStoreByteString state' outOffset bytes
    _ -> return state'

runOperation CALLCODE env state@VMState{stack=gas:to:value:inOffset:inSize:outOffset:_:rest} = do

  inputData <- liftIO $ mLoadByteString state inOffset inSize
  let address = Address $ fromIntegral to
  addressState <- getAddressState address
  code <- 
      fromMaybe B.empty <$>
                getCode (codeHash addressState)

  (nestedState, newStorageStateRoot) <-
    runCodeFromStart (callDepth state + 1)
     (fromIntegral gas)
     Environment {
       envOwner = address,
       envOrigin = envOrigin env,
       envGasPrice = envGasPrice env,
       envInputData = inputData,
       envSender = envOwner env,
       envValue = fromIntegral value,
       envCode = Code code,
       envBlock = envBlock env
       }

  let retVal = fromMaybe B.empty $ returnVal nestedState
 
  let usedGas = fromIntegral gas - vmGasRemaining nestedState

  state' <- liftIO $ mStoreByteString state outOffset retVal

  let success = 1

  addressState' <- getAddressState address
  putAddressState address addressState'{contractRoot=newStorageStateRoot}

  pay "CALLCODE fees" (envOwner env) address (fromIntegral value)

  return state'{stack=success:rest, vmGasRemaining = vmGasRemaining state' - usedGas}



runOperation CALLCODE _ state =
  return $ state { vmException=Just StackTooSmallException } 

                                                               

runOperation RETURN _ state@VMState{stack=(address:size:rest)} = do
  retVal <- liftIO $ mLoadByteString state address size
  return $ state { stack=rest, done=True, returnVal=Just retVal }

runOperation RETURN _ state =
  return $ state { vmException=Just StackTooSmallException } 

runOperation SUICIDE _ state =
  return $ state { done=True, markedForSuicide=True }


runOperation (MalformedOpcode opcode) _ state = do
  liftIO $ putStrLn $ CL.red ("Malformed Opcode: " ++ showHex opcode "")
  return state { vmException=Just MalformedOpcodeException }

runOperation x _ _ = error $ "Missing case in runOperation: " ++ show x



-------------------




movePC::VMState->Int->VMState
movePC state l = state{ pc=pc state + l }

opGasPrice::VMState->Operation->ContextM (Integer, Integer)
opGasPrice _ STOP = return (0, 0)
opGasPrice _ SUICIDE = return (0, 0)


opGasPrice _ BALANCE = return (20, 0)
opGasPrice _ SLOAD = return (20, 0)
opGasPrice _ CALL = return (20, 0)

opGasPrice _ LOG0 = return (96, 0)
opGasPrice _ LOG1 = return (96, 0)
opGasPrice _ LOG2 = return (96, 0)
opGasPrice _ LOG3 = return (128, 0)
opGasPrice _ LOG4 = return (128, 0)

opGasPrice _  CREATE = return (100, 0)

opGasPrice VMState{stack=_:size:_} SHA3 = return (10+10*ceiling(fromIntegral size/(32::Double)), 0)

opGasPrice VMState{stack=_:e:_} EXP = return (1 + ceiling (log (fromIntegral e) / log (256::Double)), 0)

opGasPrice VMState{stack=_:_:size:_} CODECOPY = return (1 + ceiling (fromIntegral size / (32::Double)), 0)
opGasPrice VMState{stack=_:_:size:_} CALLDATACOPY = return (1 + ceiling (fromIntegral size / (32::Double)), 0)
opGasPrice VMState{ stack=p:val:_ } SSTORE = do
  oldVals <- getStorageKeyVals (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
  let oldVal =
          case oldVals of
            [] -> 0::Word256
            [x] -> fromInteger $ rlpDecode $ snd x
            _ -> error "multiple values in storage"
  return $
    case (oldVal, val) of
      (0, x) | x /= 0 -> (300, 0)
      (x, 0) | x /= 0 -> (0, 100)
      _ -> (100, 0)
opGasPrice _ _ = return (1, 0)

--missing stuff
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
  (val, theRefund) <- opGasPrice state op
  return $ addToRefund theRefund $ decreaseGas val state
      where
        addToRefund::Integer->VMState->VMState
        addToRefund val state' = state'{refund=refund state' + val}

nibbleString2ByteString::N.NibbleString->B.ByteString
nibbleString2ByteString (N.EvenNibbleString s) = s
nibbleString2ByteString (N.OddNibbleString c s) = c `B.cons` s


showHex4::Int->String
showHex4 i = replicate (4 - length rawOutput) '0' ++ rawOutput
    where rawOutput = showHex i ""

formatOp::Operation->String
formatOp (PUSH x) = "PUSH" ++ show (length x) -- ++ show x
formatOp x = show x

formatAddressWithoutColor::Address->String
formatAddressWithoutColor (Address x) = padZeros 40 $ showHex x ""


runCode::Environment->VMState->Int->ContextM VMState
runCode env state c = do
  memBefore <- liftIO $ getSizeInWords $ memory state
  let (op, len) = getOperationAt (envCode env) (pc state)
  --liftIO $ putStrLn $ "EVM [ 19:22" ++ show op ++ " #" ++ show c ++ " (" ++ show (vmGasRemaining state) ++ ")"
  state' <- decreaseGasForOp op state
  result <- runOperation op env state'
  memAfter <- liftIO $ getSizeInWords $ memory result
  liftIO $ putStrLn $ "EVM [ eth | " ++ show (callDepth state) ++ " | " ++ formatAddressWithoutColor (envOwner env) ++ " | #" ++ show c ++ " | " ++ map toUpper (showHex4 (pc state)) ++ " : " ++ formatOp op ++ " | " ++ show (vmGasRemaining state) ++ " | " ++ show (vmGasRemaining result - vmGasRemaining state) ++ " | " ++ show(toInteger memAfter - toInteger memBefore) ++ "x32 ]"
  --liftIO $ putStrLn $ "EVM [ 19:23:05 | eth | " ++ show (callDepth state) ++ " | " ++ formatAddressWithoutColor (envOwner env) ++ " | #" ++ show c ++ " | " ++ map toUpper (showHex4 (pc state)) ++ " : " ++ formatOp op ++ " | " ++ show (vmGasRemaining state) ++ " | " ++ show (vmGasRemaining result - vmGasRemaining state) ++ " | " ++ show(toInteger memAfter - toInteger memBefore) ++ "x32 ]"
  memString <- liftIO $ getShow (memory result)
  memSize <- liftIO $ getSizeInBytes $ memory result
  liftIO $ putStrLn $ " > memory (" ++ showHex memSize "" ++ "): " ++ memString
  liftIO $ putStrLn "STACK"
  liftIO $ putStrLn $ unlines (("    " ++) <$> padZeros 64 <$> flip showHex "" <$> stack result)
  cxt <- get
  liftIO $ putStrLn $ "STORAGE (" ++ show (pretty $ stateRoot $ storageDB cxt) ++ ")"
  kvs <- getStorageKeyVals ""
  liftIO $ putStrLn $ unlines (map (\(k, v) -> "0x" ++ showHex (byteString2Integer $ nibbleString2ByteString k) "" ++ ": 0x" ++ showHex (rlpDecode $ rlpDeserialize $ rlpDecode v::Integer) "") kvs)
  case result of
    VMState{vmException=Just _} -> return result{ vmGasRemaining = 0 } 
    VMState{done=True} -> return $ movePC result len
    state2 -> runCode env (movePC state2 len) (c+1)

runCodeFromStart::Int->Integer->Environment->ContextM (VMState, SHAPtr)
runCodeFromStart callDepth' gasLimit' env = do
  liftIO $ putStrLn $ "running code: " ++ tab (CL.magenta ("\n" ++ show (pretty $ envCode env)))

  addressState <- getAddressState (envOwner env)
  oldStateRoot <- getStorageStateRoot
  setStorageStateRoot (contractRoot addressState)

  vmState <- liftIO startingState
  state' <- runCode env vmState{callDepth=callDepth', vmGasRemaining=gasLimit'} 0

  newStateRoot <- getStorageStateRoot
  setStorageStateRoot oldStateRoot

  liftIO $ putStrLn "VM has finished running"

  return (state', newStateRoot)


runCodeForTransaction'::Block->Int->Address->Integer->Integer->Integer->Address->Code->B.ByteString->ContextM (B.ByteString, Integer)
runCodeForTransaction' b callDepth' sender value' gasPrice' availableGas owner code theData = do

  liftIO $ putStrLn $ "availableGas: " ++ show availableGas
  pay "pre-VM fees" sender (coinbase $ blockData b) (availableGas*gasPrice')

  pay "transaction value transfer" sender owner value'

  (vmState, newStorageStateRoot) <-
    runCodeFromStart callDepth' availableGas
          Environment{
            envGasPrice=gasPrice',
            envBlock=b,
            envOwner = owner,
            envOrigin = sender,
            envInputData = theData,
            envSender = sender,
            envValue = value',
            envCode = code
            }

  liftIO $ putStrLn $ "gasRemaining: " ++ show (vmGasRemaining vmState)
  let usedGas =  - vmGasRemaining vmState - refund vmState
  liftIO $ putStrLn $ "gasUsed: " ++ show usedGas
  pay "VM refund fees" sender (coinbase $ blockData b) (usedGas * gasPrice')

  addressState <- getAddressState owner
  putAddressState owner addressState{contractRoot=newStorageStateRoot}

  case vmException vmState of
        Just e -> do
          liftIO $ putStrLn $ CL.red $ show e
          return (B.empty, vmGasRemaining vmState)

        Nothing -> do
          let result = fromMaybe B.empty $ returnVal vmState
          liftIO $ putStrLn $ "Result: " ++ show result
          liftIO $ putStrLn $ "Gas remaining: " ++ show (vmGasRemaining vmState) ++ ", needed: " ++ show (5*toInteger (B.length result))
          liftIO $ putStrLn $ show (pretty owner) ++ ": " ++ format result
          liftIO $ putStrLn $ "adding storage " ++ show (pretty newStorageStateRoot) -- stateRoot $ storageDB cxt)                                                                                          

          return (result, vmGasRemaining vmState)



--bool Executive::call(Address _receiveAddress, Address _codeAddress, Address _senderAddress, u256 _value, u256 _gasPrice, bytesConstRef _data, u256 _gas, Address _originAddress)

--bool Executive::create(Address _sender, u256 _endowment, u256 _gasPrice, u256 _gas, bytesConstRef _init, Address _origin)
create::Block->Int->Address->Integer->Integer->Integer->Address->Code->ContextM Integer
create b callDepth' sender value' gasPrice' availableGas newAddress init' = do

  (result, remainingGas) <- runCodeForTransaction' b callDepth' sender value' gasPrice' availableGas newAddress init' B.empty

  liftIO $ putStrLn $ "Result: " ++ show result
  if 5*toInteger (B.length result) < remainingGas
    then do
      pay "fee for assignment of code from init" sender (coinbase $ blockData b) (5*toInteger (B.length result)*gasPrice')
      addCode result
      addressState <- getAddressState newAddress
      putAddressState newAddress addressState{codeHash=hash result}
      return (remainingGas - 5 * toInteger (B.length result))
    else return remainingGas


nestedRun::Environment->VMState->Word256->Address->Word256->B.ByteString->ContextM (VMState, Maybe B.ByteString)
nestedRun env state gas address value inputData = do

  theBalance <- fmap balance $ getAddressState $ envOwner env

  if theBalance < fromIntegral value
    then return (state{stack=0:stack state}, Nothing)
    else do

      pay "nestedRun fees" (envOwner env) address (fromIntegral value)

      addressState <- getAddressState address
      code <-
          fromMaybe B.empty <$>
                    getCode (codeHash addressState)


      (nestedState, newStorageStateRoot) <-
          runCodeFromStart (callDepth state + 1)
                               (fromIntegral gas)
                               Environment {
                                 envOwner = address,
                                 envOrigin = envOrigin env,
                                 envGasPrice = envGasPrice env,
                                 envInputData = inputData,
                                 envSender = envOwner env,
                                 envValue = fromIntegral value,
                                 envCode = Code code,
                                 envBlock = envBlock env
                               }

      let retVal = fromMaybe B.empty $ returnVal nestedState

      {-state' <- 
        if storeRetVal 
        then do
        liftIO $ mStoreByteString state outOffset retVal
        else return state-}

      let usedGas = fromIntegral gas - vmGasRemaining nestedState
                                 
      let success = 
              case vmException nestedState of
                Nothing -> 1
                _ -> 0

      addressState' <- getAddressState address
      putAddressState address addressState'{contractRoot=newStorageStateRoot}

      return (state{stack=success:stack state, vmGasRemaining = vmGasRemaining state - usedGas}, Just retVal)


