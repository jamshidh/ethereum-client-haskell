
module Opcodes where

import Prelude hiding (LT, GT, EQ)

import Data.Binary
import qualified Data.ByteString as B
import Data.Functor
import qualified Data.Map as M

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
opCode2Op rom | B.null rom = (STOP, 1) --according to the yellowpaper, should return STOP if outside of the code bytestring
opCode2Op rom =
  case M.lookup (B.head rom) code2OpMap of
    Just (f, len) -> (f $ B.unpack $ B.take (len-1) $ B.tail rom , len)
    Nothing -> error $ "code is missing in code2OpMap: " ++ show (B.head rom)


