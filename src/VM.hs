
module VM where

import Prelude hiding (LT, GT, EQ)

import Data.Array.IO
import Data.Binary
import Data.Bits
import qualified Data.ByteString as B
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Database.LevelDB
import Network.Haskoin.Crypto (Word256)

import Address
import AddressState
import DBs
import Format
import Memory
import Util

--import Debug.Trace

data Operation = STOP | ADD | MUL | SUB | DIV | SDIV | MOD | SMOD | EXP | NEG | LT | GT | SLT | SGT | EQ | NOT | AND | OR | XOR | BYTE | SHA3 | ADDRESS | BALANCE | ORIGIN | CALLER | CALLVALUE | CALLDATALOAD | CALLDATASIZE | CALLDATACOPY | CODESIZE | CODECOPY | GASPRICE | PREVHASH | COINBASE | TIMESTAMP | NUMBER | DIFFICULTY | GASLIMIT | POP | DUP | SWAP | MLOAD | MSTORE | MSTORE8 | SLOAD | SSTORE | JUMP | JUMPI | PC | MSIZE | GAS
               | PUSH [Word8]
               | CREATE | CALL | RETURN | SUICIDE deriving (Show, Eq, Ord)

data OPData = OPData Word8 ([Word8]->Operation, Int) Int Int String

type EthCode = [Operation]

singleOp::Operation->([Word8]->Operation, Int)
singleOp o = (const o, 1)

pushOp::Int->([Word8]->Operation, Int)
pushOp numArgs = (\bytes -> PUSH (take numArgs bytes), numArgs + 1)

opDatas::[OPData]
opDatas = 
  [
    OPData 0x00 (singleOp STOP) 0 0 "Halts execution.",
    OPData 0x01 (singleOp ADD) 2 1 "Addition operation.",
    OPData 0x02 (singleOp MUL) 2 1 "Multiplication operation.",
    OPData 0x03 (singleOp SUB) 2 1 "Subtraction operation.",
    OPData 0x04 (singleOp DIV) 2 1 "Integer division operation.",
    OPData 0x05 (singleOp SDIV) 2 1 "Signed integer division operation.",
    OPData 0x06 (singleOp MOD) 2 1 "Modulo remainder operation.",
    OPData 0x07 (singleOp SMOD) 2 1 "Signed modulo remainder operation.",
    OPData 0x08 (singleOp EXP) 2 1 "Exponential operation.",
    OPData 0x09 (singleOp NEG) 1 1 "Negation operation.",
    OPData 0x0a (singleOp LT) 2 1 "Less-than comparision.",
    OPData 0x0b (singleOp GT) 2 1 "Greater-than comparision.",
    OPData 0x0c (singleOp SLT) 2 1 "Signed less-than comparision.",
    OPData 0x0d (singleOp SGT) 2 1 "Signed greater-than comparision.",
    OPData 0x0e (singleOp EQ) 2 1 "Equality comparision.",
    OPData 0x0f (singleOp NOT) 1 1 "Simple not operator.",
    OPData 0x10 (singleOp AND) 2 1 "Bitwise AND operation.",
    OPData 0x11 (singleOp OR) 2 1 "Bitwise OR operation.",
    OPData 0x12 (singleOp XOR) 2 1 "Bitwise XOR operation.",
    OPData 0x13 (singleOp BYTE) 2 1 "Retrieve single byte from word.",
    OPData 0x20 (singleOp SHA3) 2 1 "Compute SHA3-256 hash.",
    OPData 0x30 (singleOp ADDRESS) 0 1 "Get address of currently executing account.",
    OPData 0x31 (singleOp BALANCE) 1 1 "Get balance of the given account.",
    OPData 0x32 (singleOp ORIGIN) 0 1 "Get execution origination address.",
    OPData 0x33 (singleOp CALLER) 0 1 "Get caller address.",
    OPData 0x34 (singleOp CALLVALUE) 0 1 "Get deposited value by the instruction/transaction responsible for this execution.",
    OPData 0x35 (singleOp CALLDATALOAD) 1 1 "Get input data of current environment.",
    OPData 0x36 (singleOp CALLDATASIZE) 0 1 "Get size of input data in current environment.",
    OPData 0x37 (singleOp CALLDATACOPY) 3 0 "Copy input data in current environment to memory.",
    OPData 0x38 (singleOp CODESIZE) 0 1 "Get size of code running in current environment.",
    OPData 0x39 (singleOp CODECOPY) 3 0 "Copy code running in current environment to memory.",
    OPData 0x3a (singleOp GASPRICE) 0 1 "Get price of gas in current environment.",
    OPData 0x40 (singleOp PREVHASH) 0 1 "Get hash of most recent complete block.",
    OPData 0x41 (singleOp COINBASE) 0 1 "Get the block’s coinbase address.",
    OPData 0x42 (singleOp TIMESTAMP) 0 1 "Get the block’s timestamp.",
    OPData 0x43 (singleOp NUMBER) 0 1 "Get the block’s number.",
    OPData 0x44 (singleOp DIFFICULTY) 0 1 "Get the block’s difficulty.",
    OPData 0x45 (singleOp GASLIMIT) 0 1 "Get the block’s gas limit.",
    OPData 0x50 (singleOp POP) 1 0 "Remove item from stack.",
    OPData 0x51 (singleOp DUP) 1 2 "Duplicate stack item.",
    OPData 0x52 (singleOp SWAP) 2 2 "Exchange stack items.",
    OPData 0x53 (singleOp MLOAD) 1 1 "Load word from memory.",
    OPData 0x54 (singleOp MSTORE) 2 0 "Save word to memory.",
    OPData 0x55 (singleOp MSTORE8) 2 0 "Save byte to memory.",
    OPData 0x56 (singleOp SLOAD) 1 1 "Load word from storage.",
    OPData 0x57 (singleOp SSTORE) 2 0 "Save word to storage.",
    OPData 0x58 (singleOp JUMP) 1 0 "Alter the program counter.",
    OPData 0x59 (singleOp JUMPI) 2 0 "Conditionally alter the program counter.",
    OPData 0x5a (singleOp PC) 0 1 "Get the program counter.",
    OPData 0x5b (singleOp MSIZE) 0 1 "Get the size of active memory in bytes.",
    OPData 0x5c (singleOp GAS) 0 1 "Get the amount of available gas.",
    OPData 0x60 (pushOp 1) 0 1 "Place 1 byte item on stack.",
    OPData 0x61 (pushOp 2) 0 1 "Place 2-byte item on stack.",
    OPData 0x62 (pushOp 3) 0 1 "Place 3-byte item on stack.",
    OPData 0x63 (pushOp 4) 0 1 "Place 4-byte item on stack.",
    OPData 0x64 (pushOp 5) 0 1 "Place 5-byte item on stack.",
    OPData 0x65 (pushOp 6) 0 1 "Place 6-byte item on stack.",
    OPData 0x66 (pushOp 7) 0 1 "Place 7-byte item on stack.",
    OPData 0x67 (pushOp 8) 0 1 "Place 8-byte item on stack.",
    OPData 0x68 (pushOp 9) 0 1 "Place 9-byte item on stack.",
    OPData 0x69 (pushOp 10) 0 1 "Place 10-byte item on stack.",
    OPData 0x6a (pushOp 11) 0 1 "Place 11-byte item on stack.",
    OPData 0x6b (pushOp 12) 0 1 "Place 12-byte item on stack.",
    OPData 0x6c (pushOp 13) 0 1 "Place 13-byte item on stack.",
    OPData 0x6d (pushOp 14) 0 1 "Place 14-byte item on stack.",
    OPData 0x6e (pushOp 15) 0 1 "Place 15-byte item on stack.",
    OPData 0x6f (pushOp 16) 0 1 "Place 16-byte item on stack.",
    OPData 0x70 (pushOp 17) 0 1 "Place 17-byte item on stack.",
    OPData 0x71 (pushOp 18) 0 1 "Place 18-byte item on stack.",
    OPData 0x72 (pushOp 19) 0 1 "Place 19-byte item on stack.",
    OPData 0x73 (pushOp 20) 0 1 "Place 20-byte item on stack.",
    OPData 0x74 (pushOp 21) 0 1 "Place 21-byte item on stack.",
    OPData 0x75 (pushOp 22) 0 1 "Place 22-byte item on stack.",
    OPData 0x76 (pushOp 23) 0 1 "Place 23-byte item on stack.",
    OPData 0x77 (pushOp 24) 0 1 "Place 24-byte item on stack.",
    OPData 0x78 (pushOp 25) 0 1 "Place 25-byte item on stack.",
    OPData 0x79 (pushOp 26) 0 1 "Place 26-byte item on stack.",
    OPData 0x7a (pushOp 27) 0 1 "Place 27-byte item on stack.",
    OPData 0x7b (pushOp 28) 0 1 "Place 28-byte item on stack.",
    OPData 0x7c (pushOp 29) 0 1 "Place 29-byte item on stack.",
    OPData 0x7d (pushOp 30) 0 1 "Place 30-byte item on stack.",
    OPData 0x7e (pushOp 31) 0 1 "Place 31-byte item on stack.",
    OPData 0x7f (pushOp 32) 0 1 "Place 32-byte item on stack.",
    OPData 0xf0 (singleOp CREATE) 3 1 "Create a new account with associated code.",
    OPData 0xf1 (singleOp CALL) 7 1 "Message-call into an account.",
    OPData 0xf2 (singleOp RETURN) 2 0 "Halt execution returning output data.",
    OPData 0xff (singleOp SUICIDE) 1 0 "Halt execution and register account for later deletion."
  ]

--op2CodeMap::Operation->(Int->Word8)
--op2CodeMap=M.fromList $ (\(OPData code op _ _ _) -> (op, code)) <$> opDatas
code2OpMap::M.Map Word8 ([Word8] -> Operation, Int)
code2OpMap=M.fromList $ (\(OPData opcode op _ _ _) -> (opcode, op)) <$> opDatas

{-
op2OpCode::Operation->Word8
op2OpCode op =
  case M.lookup op op2CodeMap of
    Just x -> x
    Nothing -> error $ "op is missing in op2CodeMap: " ++ show op
-}

opCode2Op::B.ByteString->(Operation, Int)
opCode2Op rom =
  case M.lookup (B.head rom) code2OpMap of
    Just (f, len) -> (f $ B.unpack $ B.take (len-1) $ B.tail rom , len)
    Nothing -> error $ "code is missing in code2OpMap: " ++ show (B.head rom)


getOperationAt::B.ByteString->Int->(Operation, Int)
getOperationAt rom p = opCode2Op $ B.drop p rom

showCode::B.ByteString->String
showCode rom | B.null rom = ""
showCode rom = show op ++ "\n" ++ showCode (B.drop nextP rom)
    where
      (op, nextP) = getOperationAt rom 0

data VMError = VMError String deriving (Show)

data VMState =
  VMState {
    code::B.ByteString,
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

startingState::B.ByteString->Address->IO VMState
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

runOperation _ _ SHA3 state@VMState{stack=(x:y:rest)} = undefined

runOperation _ _ ADDRESS state = return state{stack=fromIntegral a:stack state}
    where
      Address a = address state

runOperation sdb p BALANCE state@VMState{stack=(x:rest)} = do
  maybeAddressState <- runResourceT $ do
                    getAddressState sdb p (Address $ fromIntegral x)
  return state{stack=(fromIntegral $ fromMaybe 0 $ balance <$> maybeAddressState):rest}
runOperation _ _ BALANCE state = addErr "stack did not contain enough elements" state

--ORIGIN | CALLER | CALLVALUE | CALLDATALOAD | CALLDATASIZE | CALLDATACOPY | CODESIZE | CODECOPY | GASPRICE | PREVHASH | COINBASE | TIMESTAMP | NUMBER | DIFFICULTY | GASLIMIT | 

runOperation _ _ POP state@VMState{stack=x:rest} = return state{stack=rest}
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

-- MSIZE | GAS
  


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
          [address, size] ->
            sequence $ mLoad8 (memory state) <$> fromIntegral <$> [address..address+size-1]
          [] -> return [] --Happens when STOP is called
          _ -> error $ "Error in getReturnValue: VM ended with stack in an unsupported case"
      return $ Right $ B.pack vals
  


runCodeFromStart::StateDB->SHAPtr->B.ByteString->Address->IO VMState
runCodeFromStart sdb p rom address = do
  s <- startingState rom address
  runCode sdb p s
