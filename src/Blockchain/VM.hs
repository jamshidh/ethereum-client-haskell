{-# LANGUAGE OverloadedStrings #-}

module Blockchain.VM (
  --runCodeFromStart,
  runCodeForTransaction',
  create
  ) where

import Prelude hiding (LT, GT, EQ)

import qualified Codec.Digest.SHA as SHA2
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import qualified Crypto.Hash.RIPEMD160 as RIPEMD
import Data.Binary
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import Network.Haskoin.Crypto (Word256)
import Network.Haskoin.Internals (Signature(..))
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Blockchain.Colors as CL
import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressState
import Blockchain.Data.Block
import Blockchain.Data.Code
import Blockchain.Data.Log
import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.DB.CodeDB
import Blockchain.DB.ModifyStateDB
import Blockchain.DBM
import Blockchain.ExtDBs
import Blockchain.ExtendedECDSA
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
import Blockchain.Debug

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
s256ToInteger i = toInteger i - 0x10000000000000000000000000000000000000000000000000000000000000000


swapn::Int->VMState->ContextM VMState
swapn n state@VMState{stack=v1:rest1} | length rest1 >= n = return state{stack=v2:(middle++(v1:rest2))}
    where
      (middle, v2:rest2) = splitAt (n-1) rest1
swapn _ state = return state{vmException=Just StackTooSmallException}

getByte::Word256->Word256->Word256
getByte whichByte val | whichByte < 32 = val `shiftR` (8*(31 - fromIntegral whichByte)) .&. 0xFF
getByte _ _ = 0;

signExtend::Word256->Word256->Word256
signExtend numBytes val = fromInteger prefix .|. val
  where
    lastByte = getByte numBytes val
    lastBitSet = testBit lastByte 7
    prefix = bytes2Integer $ replicate (fromIntegral $ numBytes+1) (if lastBitSet then 0xff else 0) ++ replicate (fromIntegral $ 31 - numBytes) 0

--TODO- This really should be in its own monad!
--The monad should manage everything in the VM and environment (extending the ContextM), and have pop and push operations, perhaps even automating pc incrementing, gas charges, etc.
--The code would simplify greatly, but I don't feel motivated to make the change now since things work.

runOperation::Operation->Environment->VMState->ContextM VMState
runOperation STOP _ state = return state{done=True}

runOperation ADD env state = binaryAction (+) env state
runOperation MUL env state = binaryAction (*) env state
runOperation SUB env state = binaryAction (-) env state
runOperation DIV _ state@VMState{stack=_:0:rest} = 
  return $ state { stack=0:rest }
runOperation DIV env state = binaryAction quot env state
runOperation SDIV _ state@VMState{stack=_:0:rest} = 
  return $ state { stack=0:rest } 
runOperation SDIV env state = binaryAction ((fromIntegral .) . quot `on` s256ToInteger) env state
runOperation MOD _ state@VMState{stack=_:0:rest} = 
  return $ state { stack=0:rest } 
runOperation MOD env state = binaryAction mod env state
runOperation SMOD _ state@VMState{stack=_:0:rest} = 
  return $ state { stack=0:rest } 
runOperation SMOD env state = binaryAction ((fromIntegral .) . mod `on` s256ToInteger) env state

runOperation ADDMOD _ state@VMState{stack=(v1:v2:modVal:size:rest)} = do
  let ret = (toInteger v1 + toInteger v2) `mod` toInteger modVal
  return state{stack=fromInteger ret:rest}
runOperation ADDMOD _ state = 
  return state{vmException=Just StackTooSmallException}
  
runOperation MULMOD _ state@VMState{stack=(v1:v2:modVal:rest)} = do
  let ret = (toInteger v1 * toInteger v2) `mod` toInteger modVal
  return state{stack=fromInteger ret:rest}
runOperation MULMOD _ state = 
  return state{vmException=Just StackTooSmallException}


runOperation EXP env state = binaryAction (^) env state
runOperation SIGNEXTEND env state = binaryAction signExtend env state



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

runOperation NOT env state = unaryAction (0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF `xor`) env state

runOperation BYTE env state = binaryAction getByte env state

runOperation SHA3 _ state@VMState{stack=(p:size:rest)} = do
{-  liftIO $ putStrLn "Getting ready to calculate SHA3"
  liftIO $ putStrLn $ "location = " ++ show p ++ ", size = " ++ show size

  theData <- liftIO $ mLoadByteString state p size

  liftIO $ putStrLn "data copied"
  
  let SHA theHash = hash theData
-}
  (state', theData) <- liftIO $ mLoadByteString state p size

  if isNothing $ vmException state'
    then do
    let SHA theHash = hash theData
    return state'{stack=theHash:rest}
    else return state'



runOperation ADDRESS Environment{envOwner=Address a} state = return state{stack=fromIntegral a:stack state}

runOperation BALANCE _ state@VMState{stack=(x:rest)} = do
  addressState <- lift $ getAddressState (Address $ fromIntegral x)
  return state{stack=fromIntegral (balance addressState):rest}
runOperation BALANCE env state = liftIO $ addErr "stack did not contain enough elements" (envCode env) state

runOperation ORIGIN Environment{envOrigin=Address sender} state = return state{stack=fromIntegral sender:stack state}

runOperation CALLER Environment{envSender=Address owner} state = return state{stack=fromIntegral owner:stack state}

runOperation CALLVALUE Environment{envValue=val} state = return state{stack=fromIntegral val:stack state}

runOperation CALLDATALOAD Environment{envInputData=d} state@VMState{stack=p:rest} = do

{-  liftIO $ putStrLn $ "################## " ++ show (d)
  liftIO $ putStrLn $ "################## " ++ show (B.drop (fromIntegral p) d)
  liftIO $ putStrLn $ "################## " ++ show (B.take 32 $ B.drop (fromIntegral p) d)
  liftIO $ putStrLn $ "################## " ++ show (B.unpack $ B.take 32 $ B.drop (fromIntegral p) d)
  liftIO $ putStrLn $ "################## " ++ show (appendZerosTo32 $ B.unpack $ B.take 32 $ B.drop (fromIntegral p) d)
  liftIO $ putStrLn $ "################## " ++ show (bytes2Integer $ appendZerosTo32 $ B.unpack $ B.take 32 $ B.drop (fromIntegral p) d)
-}

  let val = bytes2Integer $ appendZerosTo32 $ B.unpack $ B.take 32 $ safeDrop p $ d
  return state{stack=fromIntegral val:rest}
    where
      appendZerosTo32 x | length x < 32 = x ++ replicate (32-length x) 0
      appendZerosTo32 x = x
      
runOperation CALLDATALOAD _ s = return s{ vmException=Just StackTooSmallException } 

runOperation CALLDATASIZE Environment{envInputData=d} state = return state{stack=fromIntegral (B.length d):stack state}

runOperation CALLDATACOPY Environment{envInputData=d} state@VMState{stack=memP:codeP:size:rest} = do
  state'<-liftIO $ mStoreByteString state memP $ safeTake size $ safeDrop codeP $ d
  return state'{stack=rest}

runOperation CODESIZE Environment{envCode=c} state = return state{stack=fromIntegral (codeLength c):stack state}

runOperation CODECOPY Environment{envCode=Code c} state@VMState{stack=memP:codeP:size:rest} = do
  state' <- liftIO $ mStoreByteString state memP $ safeTake size $ safeDrop codeP $ c
  return state'{stack=rest}

runOperation GASPRICE Environment{envGasPrice=gp} state = return state{stack=fromIntegral gp:stack state}

runOperation EXTCODESIZE env state@VMState{stack=addressWord:rest} = do
  let address = Address $ fromIntegral (addressWord `mod` (2^160))
  addressState <- lift $ getAddressState address
  code <- lift $ fromMaybe B.empty <$> getCode (codeHash addressState)
  return state{stack=fromIntegral (B.length code):rest}

runOperation EXTCODESIZE _ state = 
  return state{vmException=Just StackTooSmallException}

runOperation EXTCODECOPY env state@VMState{stack=addressWord:memOffset:codeOffset:size:rest} = do
  let address = Address $ fromIntegral (addressWord `mod` (2^160))
  addressState <- lift $ getAddressState address
  code <- lift $ fromMaybe B.empty <$> getCode (codeHash addressState)
  state' <- liftIO $ mStoreByteString state memOffset (safeTake size $ safeDrop codeOffset $ code)
--  state' <- liftIO $ mStoreByteString state memOffset (B.take (fromIntegral size) $ B.drop (fromIntegral codeOffset) code)
  return state'{stack=fromIntegral (B.length code):rest}

runOperation EXTCODECOPY _ state = 
  return state{vmException=Just StackTooSmallException}

runOperation BLOCKHASH Environment{envBlock=Block{blockData=BlockData{number=blockNumber, parentHash=SHA prevHash}}} state@VMState{stack=number:rest} = do
  let SHA h = hash $ BC.pack $ show $ toInteger number
  if toInteger number >= blockNumber || toInteger number < blockNumber - 256
    then return state{stack=0:rest}
    else return state{stack=h:rest}

runOperation BLOCKHASH _ state =
  return $ state { vmException=Just StackTooSmallException } 


runOperation COINBASE Environment{envBlock=Block{blockData=BlockData{coinbase=Address cb}}} state = return state{stack=fromIntegral cb:stack state}

runOperation TIMESTAMP Environment{envBlock=Block{blockData=bd}} state = return state{stack=round (utcTimeToPOSIXSeconds $ timestamp bd):stack state}
runOperation NUMBER Environment{envBlock=Block{blockData=bd}} state = return state{stack=fromIntegral (number bd):stack state}
runOperation DIFFICULTY Environment{envBlock=Block{blockData=bd}} state = return state{stack=fromIntegral (difficulty bd):stack state}
runOperation GASLIMIT Environment{envBlock=Block{blockData=bd}} state = return state{stack=fromIntegral (gasLimit bd):stack state}

runOperation POP _ state@VMState{stack=_:rest} = return state{stack=rest}
runOperation POP env state = liftIO $ addErr "Stack did not contain any items" (envCode env) state

runOperation LOG0 env state@VMState{stack=offset:theSize:rest} = do
  (state', theData) <- liftIO $ mLoadByteString state offset theSize
  if isNothing $ vmException state'
    then do
    let newLog = Log{address=envOwner env, bloom=0, logData=theData, topics=[]}
    return state'{stack=rest, logs=newLog:logs state}
    else do
    return state'
runOperation LOG1 env state@VMState{stack=offset:theSize:topic1:rest} = do
  (state', theData) <- liftIO $ mLoadByteString state offset theSize
  if isNothing $ vmException state'
    then do
    let newLog = Log{address=envOwner env, bloom=0, logData=theData, topics=[topic1]}
    return state'{stack=rest, logs=newLog:logs state}
    else do
    return state'
runOperation LOG2 env state@VMState{stack=offset:theSize:topic1:topic2:rest} = do
  (state', theData) <- liftIO $ mLoadByteString state offset theSize
  if isNothing $ vmException state'
    then do
    let newLog = Log{address=envOwner env, bloom=0, logData=theData, topics=[topic1, topic2]}
    return state'{stack=rest, logs=newLog:logs state}
    else do
    return state'
runOperation LOG3 env state@VMState{stack=offset:theSize:topic1:topic2:topic3:rest} = do
  (state', theData) <- liftIO $ mLoadByteString state offset theSize
  if isNothing $ vmException state'
    then do
    let newLog = Log{address=envOwner env, bloom=0, logData=theData, topics=[topic1, topic2, topic3]}
    return state'{stack=rest, logs=newLog:logs state}
    else do
    return state'
runOperation LOG4 env state@VMState{stack=offset:theSize:topic1:topic2:topic3:topic4:rest} = do
  (state', theData) <- liftIO $ mLoadByteString state offset theSize
  if isNothing $ vmException state'
    then do
    let newLog = Log{address=envOwner env, bloom=0, logData=theData, topics=[topic1, topic2, topic3, topic4]}
    return state'{stack=rest, logs=newLog:logs state}
    else do
    return state'

runOperation LOG4 _ state =
  return $ state { vmException=Just StackTooSmallException } 


runOperation MLOAD _ state@VMState{stack=(p:rest)} = do
  (state', bytes) <- liftIO $ mLoad state p
  return state'{ stack=fromInteger (bytes2Integer bytes):rest }
  
runOperation MSTORE _ state@VMState{stack=(p:val:rest)} = do
  state' <- liftIO $ mStore state p val
  return state'{stack=rest}

runOperation MSTORE8 _ state@VMState{stack=(p:val:rest)} = do
  state' <- liftIO $ mStore8 state (fromIntegral p) (fromIntegral $ val .&. 0xFF)
  return state'{stack=rest}

runOperation SLOAD _ state@VMState{stack=(p:rest)} = do
  vals <- lift $ getStorageKeyVals (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
  let val = case vals of
              [] -> 0
              [x] -> fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode $ snd x
              _ -> error "Multiple values in storage"

  return $ state { stack=val:rest }
runOperation SLOAD _ state =
  return $ state { vmException=Just StackTooSmallException } 
  
runOperation SSTORE _ state@VMState{stack=(p:val:rest)} = do
  if val == 0
    then lift $ deleteStorageKey (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
    else lift $ putStorageKeyVal p val
          
  return $ state { stack=rest }
runOperation SSTORE _ state =
  return $ state { vmException=Just StackTooSmallException } 

--TODO- refactor so that I don't have to use this -1 hack
runOperation JUMP env state@VMState{stack=(p:rest)} = do
  let (destOpcode, _) = getOperationAt (envCode env) p
  if p `elem` envJumpDests env
    then return $ state { stack=rest, pc=fromIntegral p - 1} -- Subtracting 1 to compensate for the pc-increment that occurs every step.
    else return $ state { vmException=Just InvalidJump } 
runOperation JUMP _ state = return state{ vmException=Just StackTooSmallException } 

runOperation JUMPI env state@VMState{stack=(p:cond:rest)} = do
  let (destOpcode, _) = getOperationAt (envCode env) p
  case (p `elem` envJumpDests env, 0 /= cond) of
    (_, False) -> return state{ stack=rest }
    (True, _) -> return state{ stack=rest, pc=fromIntegral p - 1 }
    _ -> return state{ vmException=Just InvalidJump } 
  
runOperation JUMPI _ state =
  return $ state { vmException=Just StackTooSmallException } 

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

runOperation DUP1 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP2 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP3 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP4 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP5 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP6 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP7 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP8 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP9 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP10 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP11 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP12 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP13 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP14 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP15 _ state = return state{vmException=Just StackTooSmallException}
runOperation DUP16 _ state = return state{vmException=Just StackTooSmallException}


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
  addressState <- lift $ getAddressState $ envOwner env
  let newAddress = getNewAddress (envOwner env) (addressStateNonce addressState)

  (state', initCodeBytes) <- liftIO $ mLoadByteString state input size

  case (isNothing . vmException $ state', fromIntegral value > balance addressState) of
    (False, _) -> return state'
    (_, True) -> return state{stack=0:rest}
    _ -> do
      when debug $ liftIO $ putStrLn "transfer value"
      addToBalance (envOwner env) (-fromIntegral value)

      let initCode = Code initCodeBytes
      
      newVMState <- create (envBlock env) (callDepth state' + 1) (envOwner env) (envOrigin env) (toInteger value) (envGasPrice env) (vmGasRemaining state') newAddress initCode

      let codeBytes = fromMaybe B.empty $ returnVal newVMState

      lift $ addCode initCodeBytes

      let newAccount =
            (if B.null codeBytes then Nothing else Just newAddress,
             vmGasRemaining newVMState,
             AddressState {
               addressStateNonce=0,
               balance = fromIntegral value,
               contractRoot = emptyTriePtr,
               codeHash = hash initCodeBytes
               })



      newAddressExists <- lift $ addressStateExists newAddress
      when newAddressExists $ incrementNonce (envOwner env)

      let result =
            case vmException newVMState of
              Nothing -> 
                let Address result = newAddress
                in result
              Just _ -> 0

      return state'{stack=fromIntegral result:rest, vmGasRemaining=vmGasRemaining newVMState, newAccounts=newAccount:newAccounts state'}

runOperation CALL env state@VMState{stack=(gas:to:value:inOffset:inSize:outOffset:_:rest)} = do

  theAddressExists <- lift $ addressStateExists (Address $ fromIntegral to)

  if theAddressExists || to < 5
    then do

    (state', inputData) <- liftIO $ mLoadByteString state inOffset inSize

    case (isNothing . vmException $ state', fromIntegral gas > vmGasRemaining state) of
      (False, _) -> return state'{stack=0:rest}
      (_, True) -> return state{stack=0:rest, vmException=Just InsufficientFunds}
      _ -> do

        storageStateRoot <- lift getStorageStateRoot
        addressState <- lift $ getAddressState (envOwner env)
        lift $ putAddressState (envOwner env) addressState{contractRoot=storageStateRoot}

        (state'', retValue) <- nestedRun env state'{stack=rest} gas (Address $ fromIntegral to) (envOwner env) value inputData

        --Need to load newest stateroot in case it changed recursively within the nestedRun
        --TODO- think this one out....  There should be a cleaner way to do this.  Also, I am not sure that I am passing in storage changes to the nested calls to begin with.
        addressState <- lift $ getAddressState (envOwner env)
        lift $ setStorageStateRoot (contractRoot addressState)

        state''' <-
          case retValue of
            Just bytes -> liftIO $ mStoreByteString state'' outOffset bytes
            _ -> return state''

        return state'''

    else do
    addToBalance (envOwner env) (-fromIntegral value)
    let newAccount =
            (Just $ Address $ fromIntegral to,
             fromIntegral gas,
             AddressState {
               addressStateNonce=0,
               balance = fromIntegral value,
               contractRoot = emptyTriePtr,
               codeHash = hash B.empty
               })

    return state{stack=1:rest, newAccounts=newAccount:newAccounts state}

runOperation CALL _ state =
  return $ state { vmException=Just StackTooSmallException } 

runOperation CALLCODE env state@VMState{stack=gas:to:value:inOffset:inSize:outOffset:outSize:rest} = do
  (state', inputData) <- liftIO $ mLoadByteString state inOffset inSize
  let address = Address $ fromIntegral to

  theAddressExists <- lift $ addressStateExists address

  ownerBalance <- lift $ balance <$> getAddressState (envOwner env)

  case (ownerBalance < fromIntegral value, theAddressExists) of
    (True, _) -> return state'{stack=0:rest}
    (_, False) -> do
      state'' <- liftIO $ mStoreByteString state' outOffset (B.replicate (fromIntegral outSize) 0)
      return state''{stack=1:rest}
    _ -> do
      addressState <- lift $ getAddressState address
      code <- lift $ fromMaybe B.empty <$> getCode (codeHash addressState)

      nestedState <-
        runCodeFromStart (callDepth state' + 1)
        (fromIntegral gas)
        Environment {
          envOwner = envOwner env,
          envOrigin = envOrigin env,
          envGasPrice = envGasPrice env,
          envInputData = inputData,
          envSender = envOwner env,
          envValue = fromIntegral value,
          envCode = Code code,
          envJumpDests = getValidJUMPDESTs code,
          envBlock = envBlock env
          }

      let retVal = fromMaybe B.empty $ returnVal nestedState
 
      let usedGas = fromIntegral gas - vmGasRemaining nestedState

      state'' <- liftIO $ mStoreByteString state' outOffset retVal

      let success = 1

      addressState' <- lift $ getAddressState address

      paid <- pay "CALLCODE fees" (envOwner env) address (fromIntegral value)

      if paid
        then return state''{stack=success:rest, vmGasRemaining = vmGasRemaining state' - usedGas}
        else return state''{ vmException=Just InsufficientFunds } 


    

runOperation CALLCODE _ state =
  return $ state { vmException=Just StackTooSmallException } 

runOperation RETURN _ state@VMState{stack=(address:size:rest)} = do
  (state', retVal) <- liftIO $ mLoadByteString state address size
  return $ state' { stack=rest, done=True, returnVal=Just retVal }

runOperation RETURN _ state =
  return $ state { vmException=Just StackTooSmallException } 

runOperation SUICIDE env state@VMState{stack=address:rest, suicideList=sl} = do
  addressState <- lift $ getAddressState $ envOwner env
  let allFunds = balance addressState
  pay "transferring all funds upon suicide" (envOwner env) (Address $ fromIntegral address) allFunds
  return state{ stack=rest, done=True, suicideList=envOwner env:sl }


runOperation (MalformedOpcode opcode) _ state = do
  when debug $ liftIO $ putStrLn $ CL.red ("Malformed Opcode: " ++ showHex opcode "")
  return state { vmException=Just MalformedOpcodeException }

runOperation x _ _ = error $ "Missing case in runOperation: " ++ show x



-------------------




movePC::VMState->Word256->VMState
movePC state l = state{ pc=pc state + l }

opGasPrice::VMState->Operation->ContextM (Integer, Integer)
opGasPrice _ STOP = return (0, 0)
opGasPrice _ SUICIDE = return (0, 0)


opGasPrice _ BALANCE = return (20, 0)
opGasPrice _ SLOAD = return (20, 0)
opGasPrice _ CALL = return (20, 0)
--opGasPrice VMState{stack=value:_} CALLCODE = return (20+fromIntegral value, 0)
opGasPrice VMState{stack=value:_} CALLCODE = return (20, 0)

opGasPrice VMState{stack=_:size:_} LOG0 = return (32+fromIntegral size, 0)
opGasPrice VMState{stack=_:size:_} LOG1 = return (64+fromIntegral size, 0)
opGasPrice VMState{stack=_:size:_} LOG2 = return (96+fromIntegral size, 0)
opGasPrice VMState{stack=_:size:_} LOG3 = return (128+fromIntegral size, 0)
opGasPrice VMState{stack=_:size:_} LOG4 = return (160+fromIntegral size, 0)

opGasPrice _  CREATE = return (100, 0)

opGasPrice VMState{stack=_:size:_} SHA3 = return (10+10*ceiling(fromIntegral size/(32::Double)), 0)

opGasPrice VMState{stack=_:e:_} EXP = return (1 + ceiling (log (fromIntegral e) / log (256::Double)), 0)

opGasPrice VMState{stack=_:_:size:_} CODECOPY = return (1 + ceiling (fromIntegral size / (32::Double)), 0)
opGasPrice VMState{stack=_:_:size:_} CALLDATACOPY = return (1 + ceiling (fromIntegral size / (32::Double)), 0)
opGasPrice VMState{stack=_:_:_:size:_} EXTCODECOPY = return (1 + ceiling (fromIntegral size / (32::Double)), 0)
opGasPrice VMState{ stack=p:val:_ } SSTORE = do
  oldVals <- lift $ getStorageKeyVals (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
  let oldVal =
          case oldVals of
            [] -> 0::Word256
            [x] -> fromInteger $ rlpDecode $ snd x
            _ -> error "multiple values in storage"
  case (oldVal, val) of
      (0, x) | x /= 0 -> return (300, 0)
      (x, 0) | x /= 0 -> return (0, 100)
      _ -> return (100, 0)
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


showHex4::Word256->String
showHex4 i = replicate (4 - length rawOutput) '0' ++ rawOutput
    where rawOutput = showHex i ""

formatOp::Operation->String
formatOp (PUSH x) = "PUSH" ++ show (length x) -- ++ show x
formatOp x = show x

formatAddressWithoutColor::Address->String
formatAddressWithoutColor (Address x) = padZeros 40 $ showHex x ""

showHexU::Integer->[Char]
showHexU = map toUpper . flip showHex ""

printDebugInfo::Environment->Word256->Word256->Int->Operation->VMState->VMState->ContextM ()
printDebugInfo env memBefore memAfter c op state result = do
  liftIO $ putStrLn $ "EVM [ eth | " ++ show (callDepth state) ++ " | " ++ formatAddressWithoutColor (envOwner env) ++ " | #" ++ show c ++ " | " ++ map toUpper (showHex4 (pc state)) ++ " : " ++ formatOp op ++ " | " ++ show (vmGasRemaining state) ++ " | " ++ show (vmGasRemaining result - vmGasRemaining state) ++ " | " ++ show(toInteger memAfter - toInteger memBefore) ++ "x32 ]"
  liftIO $ putStrLn $ "EVM [ eth ] "
  memByteString <- liftIO $ getMemAsByteString (memory result)
  liftIO $ putStrLn "    STACK"
  liftIO $ putStr $ unlines (padZeros 64 <$> flip showHex "" <$> (reverse $ stack result))
  liftIO $ putStr $ "    MEMORY\n" ++ showMem 0 (B.unpack $ memByteString)
  liftIO $ putStrLn $ "    STORAGE"
  kvs <- lift $ getStorageKeyVals ""
  liftIO $ putStrLn $ unlines (map (\(k, v) -> "0x" ++ showHexU (byteString2Integer $ nibbleString2ByteString k) ++ ": 0x" ++ showHexU (rlpDecode $ rlpDeserialize $ rlpDecode v::Integer)) kvs)

runCode::Environment->VMState->Int->ContextM VMState
runCode env state c = do
  memBefore <- liftIO $ getSizeInWords $ memory state
  let (op, len) = getOperationAt (envCode env) (pc state)
  --liftIO $ putStrLn $ "EVM [ 19:22" ++ show op ++ " #" ++ show c ++ " (" ++ show (vmGasRemaining state) ++ ")"
  state' <- decreaseGasForOp op state
  result <-
    case state' of --only run op if not out of gas
      VMState{vmException=Just _} -> return state'
      _ -> runOperation op env state'

  memAfter <- liftIO $ getSizeInWords $ memory result

  when debug $ printDebugInfo env memBefore memAfter c op state result

  case result of
    VMState{vmException=Just _} -> return result -- { vmGasRemaining = 0 } 
    VMState{done=True} -> return $ movePC result len
    state2 -> runCode env (movePC state2 len) (c+1)

runCodeFromStart::Int->Integer->Environment->ContextM VMState
runCodeFromStart callDepth' gasLimit' env = do
  when debug $ liftIO $ putStrLn $ "running code: " ++ tab (CL.magenta ("\n" ++ show (pretty $ envCode env)))

  addressAlreadyExists <- lift $ addressStateExists (envOwner env)

  storageRoot <-
    if addressAlreadyExists
    then do
      addressState <- lift $ getAddressState (envOwner env)
      return $ contractRoot addressState
    else return emptyTriePtr

  oldStateRoot <- lift getStorageStateRoot
  lift $ setStorageStateRoot storageRoot

  vmState <- liftIO startingState
  state' <- runCode env vmState{callDepth=callDepth', vmGasRemaining=gasLimit'} 0

  newStorageStateRoot <- lift getStorageStateRoot

  --setStorageStateRoot oldStateRoot

  when debug $ liftIO $ putStrLn "VM has finished running"

  return state'



--bool Executive::create(Address _sender, u256 _endowment, u256 _gasPrice, u256 _gas, bytesConstRef _init, Address _origin)
create::Block->Int->Address->Address->Integer->Integer->Integer->Address->Code->ContextM VMState
create b callDepth' sender origin value' gasPrice' availableGas newAddress init' = do

  vmState <- runCodeForTransaction' b callDepth' sender origin value' gasPrice' availableGas newAddress init' B.empty

  let result = fromMaybe B.empty $ returnVal vmState
  when debug $ liftIO $ putStrLn $ "Result: " ++ show result

  --Not sure which way this is supposed to go....  I'll keep going back and forth until I figure it out
  case Just $ fromMaybe B.empty $ returnVal vmState of
  --case returnVal vmState of
    Nothing -> do
      addressState <- lift $ getAddressState newAddress
      liftIO $ do
        putStrLn $ "Deleting zombie account: " ++ show (pretty newAddress)
        putStrLn $ "Deleting zombie account: " ++ format addressState
      --deleteAddressState newAddress
      return vmState
    Just result -> do
      if 5*toInteger (B.length result) < vmGasRemaining vmState
        then do 
        lift $ addCode result
        newAddressExists <- lift $ addressStateExists newAddress
        {-when newAddressExists $ do
          addressState <- getAddressState newAddress
          putAddressState newAddress addressState{codeHash=hash result}-}
        --pay "fee for size of new contract" origin (coinbase $ blockData b) (5*toInteger (B.length result))
        --return vmState
        return vmState{vmGasRemaining=vmGasRemaining vmState - 5 * toInteger (B.length result)}
        else return vmState


ecdsaRecover::B.ByteString->B.ByteString
ecdsaRecover input =
    let h = fromInteger $ byteString2Integer $ B.take 32 input
        v = byteString2Integer $ B.take 32 $ B.drop 32 input
        r = fromInteger $ byteString2Integer $ B.take 32 $ B.drop 64 input
        s = fromInteger $ byteString2Integer $ B.take 32 $ B.drop 96 input
    in
     if (r == 0) || (v < 27) || (v > 28)
     then B.pack (replicate 32 0)
     else 
       let pubKey = getPubKeyFromSignature (ExtendedSignature (Signature r s) (v == 28)) h
       in B.pack [0,0,0,0,0,0,0,0,0,0,0,0] `B.append` BL.toStrict (encode $ pubKey2Address pubKey)     

ripemd::B.ByteString->B.ByteString
ripemd input =
  B.replicate 12 0 `B.append` RIPEMD.hash input

sha2::B.ByteString->B.ByteString
sha2 input =
    let val = fromInteger $ byteString2Integer $ B.take 32 input
    in
     SHA2.hash SHA2.SHA256 input



--bool Executive::call(Address _receiveAddress, Address _codeAddress, Address _senderAddress, u256 _value, u256 _gasPrice, bytesConstRef _data, u256 _gas, Address _originAddress)

call::Block->Int->Address->Address->Address->Word256->Word256->B.ByteString->Word256->Address->ContextM VMState
call b callDepth receiveAddress codeAddress senderAddress value gasPrice theData gas originAddress = do

  addressState <- lift $ getAddressState receiveAddress
  code <- lift $ fromMaybe B.empty <$> getCode (codeHash addressState)

  vmState <-
      runCodeFromStart callDepth (fromIntegral gas)
          Environment{
            envGasPrice=fromIntegral gasPrice,
            envBlock=b,
            envOwner = receiveAddress,
            envOrigin = originAddress,
            envInputData = theData,
            envSender = senderAddress,
            envValue = fromIntegral value,
            envCode = Code code
            }

  return vmState


runCodeForTransaction'::Block->Int->Address->Address->Integer->Integer->Integer->Address->Code->B.ByteString->ContextM VMState -- (B.ByteString, Integer, Maybe VMException)
runCodeForTransaction' b callDepth' sender origin value' gasPrice' availableGas owner code theData = do

  when debug $ liftIO $ putStrLn $ "availableGas: " ++ show availableGas

{-
  addressState <- getAddressState origin
  if balance addressState < value'
    then return VMState{vmException=Just InsufficientFunds, returnVal=Nothing, vmGasRemaining=availableGas}
    else do
-}
  --pay "transaction value transfer" origin owner value'

  vmState <-
      runCodeFromStart callDepth' availableGas
          Environment{
            envGasPrice=gasPrice',
            envBlock=b,
            envOwner = owner,
            envOrigin = origin,
            envInputData = theData,
            envSender = sender,
            envValue = value',
            envCode = code,
            envJumpDests = getValidJUMPDESTs $ codeBytes code
            }

{- -}
  newStorageStateRoot <- lift getStorageStateRoot
  ownerAddressState <- lift $ getAddressState owner
  when (isNothing $ vmException vmState) $
    lift $ putAddressState owner ownerAddressState{contractRoot=newStorageStateRoot}
{- -}

  case vmException vmState of
        Just e -> do
          when debug $ liftIO $ putStrLn $ CL.red $ show e
          return vmState

        Nothing -> do
          let result = fromMaybe B.empty $ returnVal vmState
          when debug $ liftIO $ do
            putStrLn $ "Result: " ++ format result
            putStrLn $ "Gas remaining: " ++ show (vmGasRemaining vmState) ++ ", needed: " ++ show (5*toInteger (B.length result))
            putStrLn $ show (pretty owner) ++ ": " ++ format result

          --return vmState{vmGasRemaining=vmGasRemaining vmState + refund vmState, refund=0}
          return vmState




nestedRun::Environment->VMState->Word256->Address->Address->Word256->B.ByteString->ContextM (VMState, Maybe B.ByteString)
nestedRun env state gas (Address x) _ value inputData | x > 0 && x < 4 = do
  lift $ putAddressState (Address x) blankAddressState
  pay "nestedRun fees" (envOwner env) (Address x) (fromIntegral value)

  let cost =
        case x of
          1 -> 500
          2 -> 50 + 50*(ceiling $ fromIntegral (B.length inputData)/32)
          3 -> 50 + 50*(ceiling $ fromIntegral (B.length inputData)/32)
          _ -> error $ "missing precompiled contract: " ++ show x

  if gas < cost
    then do
    pay "nestedRun fees" (envSender env) (coinbase . blockData . envBlock $ env) (fromIntegral gas*envGasPrice env)
    return (state{stack=0:stack state}, Just B.empty)
    else do
    pay "nestedRun fees" (envSender env) (coinbase . blockData . envBlock $ env) (fromIntegral cost*envGasPrice env)

    let result =
          case x of
            1 -> ecdsaRecover inputData
            2 -> sha2 inputData
            3 -> ripemd inputData

    return (state{stack=1:stack state}, Just result)

nestedRun env state gas address sender value inputData = do
  theBalance <- lift $ fmap balance $ getAddressState $ envOwner env

  if theBalance < fromIntegral value
    then do
    return (state{stack=0:stack state}, Nothing)
    else do
      pay "nestedRun fees" (envOwner env) address (fromIntegral value)

      addressState <- lift $ getAddressState address
      code <- lift $ fromMaybe B.empty <$> getCode (codeHash addressState)

      nestedState <-
          runCodeFromStart (callDepth state + 1)
                               (fromIntegral gas)
                               Environment {
                                 envOwner = address,
                                 envOrigin = envOrigin env,
                                 envGasPrice = envGasPrice env,
                                 envInputData = inputData,
                                 envSender = sender,
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

      storageStateRoot <- lift getStorageStateRoot
      addressState <- lift $ getAddressState address
      lift $ putAddressState address addressState{contractRoot=storageStateRoot}

      return
        (
          state{
             logs=logs nestedState ++ logs state,
             stack=success:stack state,
             vmGasRemaining = vmGasRemaining state - usedGas,
             refund= refund state + if isNothing (vmException nestedState) then refund nestedState else 0
             },
          Just retVal
        )


