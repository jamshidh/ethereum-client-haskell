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
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import qualified Crypto.Hash.RIPEMD160 as RIPEMD
import Data.Binary hiding (get, put)
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
import Network.Haskoin.Crypto (Word256, Word160)
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
import Blockchain.VM.VMM
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

binaryAction::(Word256->Word256->Word256)->VMM ()
binaryAction action = do
  x <- pop
  y <- pop
  push $ x `action` y

unaryAction::(Word256->Word256)->VMM ()
unaryAction action = do
  x <- pop
  push $ action x

pushEnvVar::Word256Storable a=>(Environment->a)->VMM ()
pushEnvVar f = do
  VMState{environment=env} <- lift get
  push $ f env

pushVMStateVar::Word256Storable a=>(VMState->a)->VMM ()
pushVMStateVar f = do
  state' <- lift get::VMM VMState
  push $ f state'

logN::Int->VMM ()
logN n = do
  offset <- pop
  theSize <- pop
  owner <- getEnvVar envOwner
  topics' <- sequence $ replicate n pop
  
  theData <- mLoadByteString offset theSize
  addLog Log{address=owner, bloom=0, logData=theData, topics=topics'}



dupN::Int->VMM ()
dupN n = do
  state <- lift get
  if length (stack state) < n
    then do
    state <- lift get
    left $ StackTooSmallException state
    else push $ stack state !! (n-1)


s256ToInteger::Word256->Integer
s256ToInteger i | i < 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF = toInteger i
s256ToInteger i = toInteger i - 0x10000000000000000000000000000000000000000000000000000000000000000


swapn::Int->VMM ()
swapn n = do
  v1 <- pop
  state <- lift get
  if length (stack state) < n + 1
    then do
    state <- lift get
    left $ StackTooSmallException state
    else do
    let (middle, v2:rest2) = splitAt (n-1) $ tail (stack state)
    lift $ put state{stack = v2:(middle++(v1:rest2))}

getByte::Word256->Word256->Word256
getByte whichByte val | whichByte < 32 = val `shiftR` (8*(31 - fromIntegral whichByte)) .&. 0xFF
getByte _ _ = 0;

signExtend::Word256->Word256->Word256
signExtend numBytes val = fromInteger prefix .|. val
  where
    lastByte = getByte numBytes val
    lastBitSet = testBit lastByte 7
    prefix = bytes2Integer $ replicate (fromIntegral $ numBytes+1) (if lastBitSet then 0xff else 0) ++ replicate (fromIntegral $ 31 - numBytes) 0

safe_div _ 0 = 0
safe_div x y = x `div` y

safe_quot _ 0 = 0
safe_quot x y = x `quot` y

safe_mod _ 0 = 0
safe_mod x y = x `mod` y


--TODO- This really should be in its own monad!
--The monad should manage everything in the VM and environment (extending the ContextM), and have pop and push operations, perhaps even automating pc incrementing, gas charges, etc.
--The code would simplify greatly, but I don't feel motivated to make the change now since things work.

runOperation::Operation->VMM ()
runOperation STOP = do
  state <- lift get
  lift $ put state{done=True}

runOperation ADD = binaryAction (+)
runOperation MUL = binaryAction (*)
runOperation SUB = binaryAction (-)
runOperation DIV = binaryAction safe_quot
runOperation SDIV = binaryAction ((fromIntegral .) . safe_quot `on` s256ToInteger)
runOperation MOD = binaryAction safe_mod
runOperation SMOD = binaryAction ((fromIntegral .) . safe_mod `on` s256ToInteger)

runOperation ADDMOD = do
  v1 <- pop::VMM Word256
  v2 <- pop::VMM Word256
  modVal <- pop::VMM Word256
  size <- pop::VMM Word256

  push $ (toInteger v1 + toInteger v2) `mod` toInteger modVal

runOperation MULMOD = do
  v1 <- pop::VMM Word256
  v2 <- pop::VMM Word256
  modVal <- pop::VMM Word256

  let ret = (toInteger v1 * toInteger v2) `mod` toInteger modVal
  push ret


runOperation EXP = binaryAction (^)
runOperation SIGNEXTEND = binaryAction signExtend



runOperation NEG = unaryAction negate
runOperation LT = binaryAction ((bool2Word256 .) . (<))
runOperation GT = binaryAction ((bool2Word256 .) . (>))
runOperation SLT = binaryAction ((bool2Word256 .) . ((<) `on` s256ToInteger))
runOperation SGT = binaryAction ((bool2Word256 .) . ((>) `on` s256ToInteger))
runOperation EQ = binaryAction ((bool2Word256 .) . (==))
runOperation ISZERO = unaryAction (bool2Word256 . (==0))
runOperation AND = binaryAction (.&.)
runOperation OR = binaryAction (.|.)
runOperation XOR = binaryAction xor

runOperation NOT = unaryAction (0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF `xor`)

runOperation BYTE = binaryAction getByte

runOperation SHA3 = do
  p <- pop
  size <- pop
  theData <- mLoadByteString p size
  let SHA theHash = hash theData
  push $ theHash

runOperation ADDRESS = pushEnvVar envOwner

runOperation BALANCE = do
  x <- pop
  addressState <- lift $ lift $ lift $ getAddressState x
  push $ balance addressState

runOperation ORIGIN = pushEnvVar envOrigin
runOperation CALLER = pushEnvVar envSender
runOperation CALLVALUE = pushEnvVar envValue

runOperation CALLDATALOAD = do
  p <- pop
  d <- getEnvVar envInputData

  let val = bytes2Integer $ appendZerosTo32 $ B.unpack $ B.take 32 $ safeDrop p $ d
  push val
    where
      appendZerosTo32 x | length x < 32 = x ++ replicate (32-length x) 0
      appendZerosTo32 x = x
      
runOperation CALLDATASIZE = pushEnvVar (B.length . envInputData)

runOperation CALLDATACOPY = do
  memP <- pop
  codeP <- pop
  size <- pop
  d <- getEnvVar envInputData
  
  mStoreByteString memP $ safeTake size $ safeDrop codeP $ d

runOperation CODESIZE = pushEnvVar (codeLength . envCode)

runOperation CODECOPY = do
  memP <- pop
  codeP <- pop
  size <- pop
  Code c <- getEnvVar envCode
  
  mStoreByteString memP $ safeTake size $ safeDrop codeP $ c

runOperation GASPRICE = pushEnvVar envGasPrice


--TODO- add code to automatically `mod` (2^160) addresses
runOperation EXTCODESIZE = do
  addressWord <- pop
  let address = Address $ fromIntegral (addressWord `mod` (2^160)::Word256)
  addressState <- lift $ lift $ lift $ getAddressState address
  code <- lift $ lift $ lift $ fromMaybe B.empty <$> getCode (codeHash addressState)
  push $ (fromIntegral (B.length code)::Word256)

runOperation EXTCODECOPY = do
  addressWord <- pop
  memOffset <- pop
  codeOffset <- pop
  size <- pop
  
  let address = Address $ (fromIntegral (addressWord `mod` (2^160)::Word256))
  addressState <- lift $ lift $ lift $ getAddressState address
  code <- lift $ lift $ lift $ fromMaybe B.empty <$> getCode (codeHash addressState)
  mStoreByteString memOffset (safeTake size $ safeDrop codeOffset $ code)
  push $ (fromIntegral (B.length code)::Word256)

runOperation BLOCKHASH = do
  number' <- pop::VMM Word256
  block <- getEnvVar envBlock
  
  let SHA h = hash $ BC.pack $ show $ toInteger number'

  let blockNumber = number (blockData block)
      
  if toInteger number' >= blockNumber || toInteger number' < blockNumber - 256
    then push (0::Word256)
    else push h

runOperation COINBASE = pushEnvVar (coinbase . blockData . envBlock)
runOperation TIMESTAMP = do
  VMState{environment=env} <- lift get
  push $ ((round . utcTimeToPOSIXSeconds . timestamp . blockData . envBlock) env::Word256)


  
runOperation NUMBER = pushEnvVar (number . blockData . envBlock)
runOperation DIFFICULTY = pushEnvVar (difficulty . blockData . envBlock)
runOperation GASLIMIT = pushEnvVar (gasLimit . blockData . envBlock)

runOperation POP = do
  _ <- pop::VMM Word256
  return ()

runOperation LOG0 = logN 0
runOperation LOG1 = logN 1
runOperation LOG2 = logN 2
runOperation LOG3 = logN 3
runOperation LOG4 = logN 4

runOperation MLOAD = do
  p <- pop
  bytes <- mLoad p
  push $ (fromInteger (bytes2Integer bytes)::Word256)
  
runOperation MSTORE = do
  p <- pop
  val <- pop
  mStore p val

runOperation MSTORE8 = do
  p <- pop
  val <- pop::VMM Word256
  mStore8 p (fromIntegral $ val .&. 0xFF)

runOperation SLOAD = do
  p <- pop
  vals <- lift $ lift $ lift $ getStorageKeyVals (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
  let val = case vals of
              [] -> 0::Word256
              [x] -> fromInteger $ rlpDecode $ rlpDeserialize $ rlpDecode $ snd x
              _ -> error "Multiple values in storage"

  push val
  
runOperation SSTORE = do
  p <- pop
  val <- pop
  if val == 0
    then lift $ lift $ lift $ deleteStorageKey (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
    else lift $ lift $ lift $ putStorageKeyVal p val

--TODO- refactor so that I don't have to use this -1 hack
runOperation JUMP = do
  p <- pop
  jumpDests <- getEnvVar envJumpDests
  theCode <- getEnvVar envCode

  --let (destOpcode, _) = getOperationAt theCode p
  if p `elem` jumpDests
    then setPC $ fromIntegral p - 1 -- Subtracting 1 to compensate for the pc-increment that occurs every step.
    else do
         state <- lift get
         left $ InvalidJump state

runOperation JUMPI = do
  p <- pop
  cond <- pop
  jumpDests <- getEnvVar envJumpDests
  theCode <- getEnvVar envCode
  
  case (p `elem` jumpDests, (0::Word256) /= cond) of
    (_, False) -> return ()
    (True, _) -> setPC $ fromIntegral p - 1
    _ -> do
      state <- lift get
      left $ InvalidJump state
  
runOperation PC = pushVMStateVar pc

runOperation MSIZE = do
  memSize <- getSizeInBytes
  push memSize

runOperation GAS = pushVMStateVar vmGasRemaining

runOperation JUMPDEST = return ()

runOperation (PUSH vals) =
  push $ (fromIntegral (bytes2Integer vals)::Word256)

runOperation DUP1 = dupN 1
runOperation DUP2 = dupN 2
runOperation DUP3 = dupN 3
runOperation DUP4 = dupN 4
runOperation DUP5 = dupN 5
runOperation DUP6 = dupN 6
runOperation DUP7 = dupN 7
runOperation DUP8 = dupN 8
runOperation DUP9 = dupN 9
runOperation DUP10 = dupN 10
runOperation DUP11 = dupN 11
runOperation DUP12 = dupN 12
runOperation DUP13 = dupN 13
runOperation DUP14 = dupN 14
runOperation DUP15 = dupN 15
runOperation DUP16 = dupN 16

runOperation SWAP1 = swapn 1
runOperation SWAP2 = swapn 2
runOperation SWAP3 = swapn 3
runOperation SWAP4 = swapn 4
runOperation SWAP5 = swapn 5
runOperation SWAP6 = swapn 6
runOperation SWAP7 = swapn 7
runOperation SWAP8 = swapn 8
runOperation SWAP9 = swapn 9
runOperation SWAP10 = swapn 10
runOperation SWAP11 = swapn 11
runOperation SWAP12 = swapn 12
runOperation SWAP13 = swapn 13
runOperation SWAP14 = swapn 14
runOperation SWAP15 = swapn 15
runOperation SWAP16 = swapn 16

runOperation CREATE = do
  value <- pop::VMM Word256
  input <- pop
  size <- pop

  owner <- getEnvVar envOwner
  block <- getEnvVar envBlock
  
  addressState <- lift $ lift $ lift $ getAddressState owner
  let newAddress = getNewAddress owner (addressStateNonce addressState)

  initCodeBytes <- mLoadByteString input size

  if fromIntegral value > balance addressState
    then push (0::Word256)
    else do
      when debug $ liftIO $ putStrLn "transfer value"
      lift $ lift $ addToBalance owner (-fromIntegral value)

      let initCode = Code initCodeBytes
      
      origin <- getEnvVar envOrigin
      gasPrice <- getEnvVar envGasPrice

      gasRemaining <- getGasRemaining

      callDepth <- getCallDepth

      newVMStateOrException <- lift $ lift $ create block callDepth owner origin (toInteger value) gasPrice gasRemaining newAddress initCode

      case newVMStateOrException of
        Left e -> do
          push (0::Word256)
        Right newVMState -> do
          let codeBytes = fromMaybe B.empty $ returnVal newVMState

          lift $ lift $ lift $ addCode initCodeBytes

          let newAccount =
                (if B.null codeBytes then Nothing else Just newAddress,
                 vmGasRemaining newVMState,
                 AddressState {
                   addressStateNonce=0,
                   balance = fromIntegral value,
                   contractRoot = emptyTriePtr,
                   codeHash = hash initCodeBytes
                   })

          newAddressExists <- lift $ lift $ lift $ addressStateExists newAddress
          when newAddressExists $ lift $ lift $ incrementNonce owner

          let result =
                case vmException newVMState of
                  Nothing -> 
                    let Address result = newAddress
                    in result::Word160
                  Just _ -> 0

          setGasRemaining $ vmGasRemaining newVMState

          addNewAccount newAccount
      
          push $ (fromIntegral result::Word256)

runOperation CALL = do
  gas <- pop
  Address to <- pop
  value <- pop::VMM Word256
  inOffset <- pop
  inSize <- pop
  outOffset <- pop
  outSize <- pop::VMM Word256

  owner <- getEnvVar envOwner
  
  theAddressExists <- lift $ lift $ lift $ addressStateExists (Address $ fromIntegral to)

  result <- 
      lift $ runEitherT $ 
           do
             when (not theAddressExists && to > 4) $ do
                      lift $ lift $ addToBalance owner (-fromIntegral value)
                      let newAccount =
                              (Just $ Address $ fromIntegral to,
                               fromIntegral gas,
                               AddressState {
                                 addressStateNonce=0,
                                 balance = fromIntegral value,
                                 contractRoot = emptyTriePtr,
                                 codeHash = hash B.empty
                               })
                      addNewAccount newAccount
                      state'' <- lift get
                      left $ AddressDoesNotExist state''

             inputData <- mLoadByteString inOffset inSize

             gasRemaining <- getGasRemaining

             --pay' "gas payment in CALL opcode run" owner (Address to) $ fromIntegral value

             storageStateRoot <- lift $ lift $ lift getStorageStateRoot
             addressState <- lift $ lift $ lift $ getAddressState owner
             lift $ lift $ lift $ putAddressState owner addressState{contractRoot=storageStateRoot}

             state' <- lift get

             useGas $ fromIntegral gas
    
             result <- lift $ lift $  nestedRun state' gas (Address to) owner value inputData

             case result of
               Right (state'', retValue) -> do
                           --Need to load newest stateroot in case it changed recursively within the nestedRun
                           --TODO- think this one out....  There should be a cleaner way to do this.  Also, I am not sure that I am passing in storage changes to the nested calls to begin with.
                           addressState <- lift $ lift $ lift $ getAddressState owner
                           lift $ lift $ lift $ setStorageStateRoot (contractRoot addressState)

                           forM_ (reverse $ logs state'') $ \log -> addLog log

                           useGas (- vmGasRemaining state'')

                           case retValue of
                             Just bytes -> mStoreByteString outOffset bytes
                             _ -> return ()


               Left e -> do
                      --useGas (- vmGasRemaining (eState e))
                      left e
    
  case result of
    Left _ ->  push (0::Word256)
    Right _ -> push (1::Word256)


runOperation CALLCODE = do

  gas <- pop::VMM Word256
  to <- pop
  value <- pop::VMM Word256
  inOffset <- pop
  inSize <- pop
  outOffset <- pop
  outSize <- pop::VMM Word256

  owner <- getEnvVar envOwner
  origin <- getEnvVar envOrigin
  gasPrice <- getEnvVar envGasPrice
  block <- getEnvVar envBlock

  inputData <- mLoadByteString inOffset inSize
  let address = to

  theAddressExists <- lift $ lift $ lift $ addressStateExists address

  ownerBalance <- lift $ lift $ lift $ balance <$> getAddressState owner

  case (ownerBalance < fromIntegral value, theAddressExists) of
    (True, _) -> push (0::Word256)
    (_, False) -> do
      mStoreByteString outOffset (B.replicate (fromIntegral outSize) 0)
      push (1::Word256)
    _ -> do
      addressState <- lift $ lift $ lift $ getAddressState address
      code <- lift $  lift $ lift $ fromMaybe B.empty <$> getCode (codeHash addressState)

      callDepth' <- getCallDepth

      nestedStateOrException <-
        lift $ lift $ runCodeFromStart (callDepth' + 1)
        (fromIntegral gas)
        Environment {
          envOwner = owner,
          envOrigin = origin,
          envGasPrice = gasPrice,
          envInputData = inputData,
          envSender = owner,
          envValue = fromIntegral value,
          envCode = Code code,
          envJumpDests = getValidJUMPDESTs code,
          envBlock = block
          }

      case nestedStateOrException of
        Left e -> do
          push (1::Word256)
          let usedGas = fromIntegral gas - vmGasRemaining (eState e)
          paid <- lift $ lift $ pay "CALLCODE fees" owner address (fromIntegral value)

          if paid
            then do
            useGas usedGas
            else left $ InsufficientFunds $ eState e
          
        Right nestedState -> do
          let retVal = fromMaybe B.empty $ returnVal nestedState
          let usedGas = fromIntegral gas - vmGasRemaining nestedState

          mStoreByteString outOffset retVal

          let success = 1::Word256

          addressState' <- lift $ lift $ lift $ getAddressState address
 
          paid <- lift $ lift $ pay "CALLCODE fees" owner address (fromIntegral value)

          if paid
            then do
            push success
            useGas usedGas
            else left $ InsufficientFunds nestedState

runOperation RETURN = do
  address <- pop
  size <- pop
  
  retVal <- mLoadByteString address size
  setDone True
  setReturnVal $ Just retVal

runOperation SUICIDE = do
  address <- pop
  owner <- getEnvVar envOwner
  addressState <- lift $ lift $ lift $ getAddressState $ owner
  owner <- getEnvVar envOwner

  let allFunds = balance addressState
  lift $ lift $ pay "transferring all funds upon suicide" owner address allFunds
  addSuicideList owner
  setDone True


runOperation (MalformedOpcode opcode) = do
  when debug $ liftIO $ putStrLn $ CL.red ("Malformed Opcode: " ++ showHex opcode "")
  state <- lift get
  left $ MalformedOpcodeException state

runOperation x = error $ "Missing case in runOperation: " ++ show x



-------------------




movePC::VMState->Word256->VMState
movePC state l = state{ pc=pc state + l }

opGasPrice::Operation->VMM (Integer, Integer)
opGasPrice STOP = return (0, 0)
opGasPrice SUICIDE = return (0, 0)


opGasPrice BALANCE = return (20, 0)
opGasPrice SLOAD = return (20, 0)
opGasPrice CALL = return (20, 0)
--opGasPrice VMState{stack=value:_} CALLCODE = return (20+fromIntegral value, 0)
opGasPrice CALLCODE = return (20, 0)

opGasPrice LOG0 = do
  size <- getStackItem 1::VMM Word256
  return (32+fromIntegral size, 0)
opGasPrice LOG1 = do
  size <- getStackItem 1::VMM Word256
  return (64+fromIntegral size, 0)
opGasPrice LOG2 = do
  size <- getStackItem 1::VMM Word256
  return (96+fromIntegral size, 0)
opGasPrice LOG3 = do
  size <- getStackItem 1::VMM Word256
  return (128+fromIntegral size, 0)
opGasPrice LOG4 = do
  size <- getStackItem 1::VMM Word256
  return (160+fromIntegral size, 0)

opGasPrice CREATE = return (100, 0)

opGasPrice SHA3 = do
  size <- getStackItem 1::VMM Word256
  return (10+10*ceiling(fromIntegral size/(32::Double)), 0)

opGasPrice EXP = do
    e <- getStackItem 1::VMM Word256
    return (1 + ceiling (log (fromIntegral e) / log (256::Double)), 0)

opGasPrice CODECOPY = do
    size <- getStackItem 2::VMM Word256
    return (1 + ceiling (fromIntegral size / (32::Double)), 0)
opGasPrice CALLDATACOPY = do
    size <- getStackItem 2::VMM Word256
    return (1 + ceiling (fromIntegral size / (32::Double)), 0)
opGasPrice EXTCODECOPY = do
    size <- getStackItem 3::VMM Word256
    return (1 + ceiling (fromIntegral size / (32::Double)), 0)
opGasPrice SSTORE = do
  p <- getStackItem 0
  val <- getStackItem 1
  oldVals <- lift $ lift $ lift $ getStorageKeyVals (N.pack $ (N.byte2Nibbles =<<) $ word256ToBytes p)
  let oldVal =
          case oldVals of
            [] -> 0::Word256
            [x] -> fromInteger $ rlpDecode $ snd x
            _ -> error "multiple values in storage"
  case (oldVal, val) of
      (0, x) | x /= (0::Word256) -> return (300, 0)
      (x, 0) | x /= 0 -> return (0, 100)
      _ -> return (100, 0)
opGasPrice _ = return (1, 0)

--missing stuff
--Glog 1 Partial payment for a LOG operation.
--Glogdata 1 Paid for each byte in a LOG operation’s data.
--Glogtopic 1 Paid for each topic of a LOG operation.









decreaseGas::Integer->VMM ()
decreaseGas val = do
  state' <- lift get
  if val <= vmGasRemaining state'
    then lift $ put state'{ vmGasRemaining = vmGasRemaining state' - val }
    else do
      lift $ put state'{ vmGasRemaining = 0 }
      left $ OutOfGasException state' 

decreaseGasForOp::Operation->VMM ()
decreaseGasForOp op = do
  (val, theRefund) <- opGasPrice op
  decreaseGas val
  addToRefund theRefund

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

runCode::Int->VMM ()
runCode c = do
  memBefore <- getSizeInWords
  code <- getEnvVar envCode

  state <- lift get

  let (op, len) = getOperationAt code (pc state)
  --liftIO $ putStrLn $ "EVM [ 19:22" ++ show op ++ " #" ++ show c ++ " (" ++ show (vmGasRemaining state) ++ ")"

  decreaseGasForOp op
  
  runOperation op

  memAfter <- getSizeInWords

  result <- lift get
  when debug $ lift $ lift $ printDebugInfo (environment result) memBefore memAfter c op state result

  case result of
    VMState{done=True} -> incrementPC len
    state2 -> do
      incrementPC len
      runCode (c+1)

runCodeFromStart::Int->Integer->Environment->ContextM (Either VMException VMState)
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

  vmState <- liftIO $ startingState env

  result <- runStateT (runEitherT (runCode 0)) vmState{callDepth=callDepth', vmGasRemaining=gasLimit'}

  newStorageStateRoot <- lift getStorageStateRoot

  --setStorageStateRoot oldStateRoot

  when debug $ liftIO $ putStrLn "VM has finished running"

  case result of
    (Left e, _) -> return $ Left e
    (_, state) -> return $ Right state



--bool Executive::create(Address _sender, u256 _endowment, u256 _gasPrice, u256 _gas, bytesConstRef _init, Address _origin)
create::Block->Int->Address->Address->Integer->Integer->Integer->Address->Code->ContextM (Either VMException VMState)
create b callDepth' sender origin value' gasPrice' availableGas newAddress init' = do

  vmStateOrException <- runCodeForTransaction' b callDepth' sender origin value' gasPrice' availableGas newAddress init' B.empty


  case vmStateOrException of
    Left e -> return $ Left e
    Right vmState -> do
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
          return $ Right vmState
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
            return $ Right vmState{vmGasRemaining=vmGasRemaining vmState - 5 * toInteger (B.length result)}
            else return $ Right vmState


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

call::Block->Int->Address->Address->Address->Word256->Word256->B.ByteString->Word256->Address->ContextM (Either VMException VMState)
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


runCodeForTransaction'::Block->Int->Address->Address->Integer->Integer->Integer->Address->Code->B.ByteString->ContextM (Either VMException VMState)
runCodeForTransaction' b callDepth' sender origin value' gasPrice' availableGas owner code theData = do

  when debug $ liftIO $ putStrLn $ "availableGas: " ++ show availableGas

{-
  addressState <- getAddressState origin
  if balance addressState < value'
    then return VMState{vmException=Just InsufficientFunds, returnVal=Nothing, vmGasRemaining=availableGas}
    else do
-}
  --pay "transaction value transfer" origin owner value'

  vmStateOrException <-
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
{- -}

  case vmStateOrException of
        Left e -> do
          when debug $ liftIO $ do
                    putStrLn $ CL.red $ format e
          return $ Left e

        Right vmState -> do
          lift $ putAddressState owner ownerAddressState{contractRoot=newStorageStateRoot}
          let result = fromMaybe B.empty $ returnVal vmState
          when debug $ liftIO $ do
            putStrLn $ "Result: " ++ format result
            putStrLn $ "Gas remaining: " ++ show (vmGasRemaining vmState) ++ ", needed: " ++ show (5*toInteger (B.length result))
            putStrLn $ show (pretty owner) ++ ": " ++ format result

          --return vmState{vmGasRemaining=vmGasRemaining vmState + refund vmState, refund=0}
          return $ Right vmState




nestedRun::VMState->Word256->Address->Address->Word256->B.ByteString->ContextM (Either VMException (VMState, Maybe B.ByteString))
nestedRun state gas (Address x) _ value inputData | x > 0 && x < 4 = do
  let env = environment state
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
    return $ Right (state{stack=0:stack state}, Just B.empty)
    else do
    pay "nestedRun fees" (envSender env) (coinbase . blockData . envBlock $ env) (fromIntegral cost*envGasPrice env)

    let result =
          case x of
            1 -> ecdsaRecover inputData
            2 -> sha2 inputData
            3 -> ripemd inputData

    return $ Right (state{stack=1:stack state}, Just result)

nestedRun state gas address sender value inputData = do
  let env = environment state
  theBalance <- lift $ fmap balance $ getAddressState $ envOwner env

  if theBalance < fromIntegral value
    then do
    return $ Right (state{stack=0:stack state}, Nothing)
    else do
      pay "nestedRun fees" (envOwner env) address (fromIntegral value)

      addressState <- lift $ getAddressState address
      code <- lift $ fromMaybe B.empty <$> getCode (codeHash addressState)

      maybeNestedState <-
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

      case maybeNestedState of
        Left e -> return $ Left e
        Right nestedState -> do
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

          let newState = state{
                logs=logs nestedState ++ logs state,
                stack=success:stack state,
                vmGasRemaining = vmGasRemaining state - usedGas,
                refund= refund state + if isNothing (vmException nestedState) then refund nestedState else 0
                }
                         
          return $ Right (nestedState, Just retVal)


