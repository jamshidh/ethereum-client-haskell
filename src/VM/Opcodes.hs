
module VM.Opcodes where

import Prelude hiding (LT, GT, EQ)

import Data.Binary
import qualified Data.ByteString as B
import Data.Functor
import qualified Data.Map as M
import Data.Maybe

import ExtWord

--import Debug.Trace

data Operation = 
    STOP | ADD | MUL | SUB | DIV | SDIV | MOD | SMOD | EXP | NEG | LT | GT | SLT | SGT | EQ | NOT | AND | OR | XOR | BYTE | SHA3 | 
    ADDRESS | BALANCE | ORIGIN | CALLER | CALLVALUE | CALLDATALOAD | CALLDATASIZE | CALLDATACOPY | CODESIZE | CODECOPY | GASPRICE | 
    PREVHASH | COINBASE | TIMESTAMP | NUMBER | DIFFICULTY | GASLIMIT | POP | DUP | SWAP | MLOAD | MSTORE | MSTORE8 | SLOAD | SSTORE | 
    JUMP | JUMPI | PC | MSIZE | GAS | 
    PUSH [Word8] | 
    CREATE | CALL | RETURN | SUICIDE |
    --Pseudo Opcodes
    LABEL String | PUSHLABEL String | PUSHDIFF String String deriving (Show, Eq, Ord)

data OPData = OPData Word8 Operation Int Int String

type EthCode = [Operation]

singleOp::Operation->([Word8]->Operation, Int)
singleOp o = (const o, 1)

pushOp::Int->([Word8]->Operation, Int)
pushOp numArgs = (\bytes -> PUSH (take numArgs bytes), numArgs + 1)

opDatas::[OPData]
opDatas = 
  [
    OPData 0x00 STOP 0 0 "Halts execution.",
    OPData 0x01 ADD 2 1 "Addition operation.",
    OPData 0x02 MUL 2 1 "Multiplication operation.",
    OPData 0x03 SUB 2 1 "Subtraction operation.",
    OPData 0x04 DIV 2 1 "Integer division operation.",
    OPData 0x05 SDIV 2 1 "Signed integer division operation.",
    OPData 0x06 MOD 2 1 "Modulo remainder operation.",
    OPData 0x07 SMOD 2 1 "Signed modulo remainder operation.",
    OPData 0x08 EXP 2 1 "Exponential operation.",
    OPData 0x09 NEG 1 1 "Negation operation.",
    OPData 0x0a LT 2 1 "Less-than comparision.",
    OPData 0x0b GT 2 1 "Greater-than comparision.",
    OPData 0x0c SLT 2 1 "Signed less-than comparision.",
    OPData 0x0d SGT 2 1 "Signed greater-than comparision.",
    OPData 0x0e EQ 2 1 "Equality comparision.",
    OPData 0x0f NOT 1 1 "Simple not operator.",
    OPData 0x10 AND 2 1 "Bitwise AND operation.",
    OPData 0x11 OR 2 1 "Bitwise OR operation.",
    OPData 0x12 XOR 2 1 "Bitwise XOR operation.",
    OPData 0x13 BYTE 2 1 "Retrieve single byte from word.",
    OPData 0x20 SHA3 2 1 "Compute SHA3-256 hash.",
    OPData 0x30 ADDRESS 0 1 "Get address of currently executing account.",
    OPData 0x31 BALANCE 1 1 "Get balance of the given account.",
    OPData 0x32 ORIGIN 0 1 "Get execution origination address.",
    OPData 0x33 CALLER 0 1 "Get caller address.",
    OPData 0x34 CALLVALUE 0 1 "Get deposited value by the instruction/transaction responsible for this execution.",
    OPData 0x35 CALLDATALOAD 1 1 "Get input data of current environment.",
    OPData 0x36 CALLDATASIZE 0 1 "Get size of input data in current environment.",
    OPData 0x37 CALLDATACOPY 3 0 "Copy input data in current environment to memory.",
    OPData 0x38 CODESIZE 0 1 "Get size of code running in current environment.",
    OPData 0x39 CODECOPY 3 0 "Copy code running in current environment to memory.",
    OPData 0x3a GASPRICE 0 1 "Get price of gas in current environment.",
    OPData 0x40 PREVHASH 0 1 "Get hash of most recent complete block.",
    OPData 0x41 COINBASE 0 1 "Get the block’s coinbase address.",
    OPData 0x42 TIMESTAMP 0 1 "Get the block’s timestamp.",
    OPData 0x43 NUMBER 0 1 "Get the block’s number.",
    OPData 0x44 DIFFICULTY 0 1 "Get the block’s difficulty.",
    OPData 0x45 GASLIMIT 0 1 "Get the block’s gas limit.",
    OPData 0x50 POP 1 0 "Remove item from stack.",
    OPData 0x51 DUP 1 2 "Duplicate stack item.",
    OPData 0x52 SWAP 2 2 "Exchange stack items.",
    OPData 0x53 MLOAD 1 1 "Load word from memory.",
    OPData 0x54 MSTORE 2 0 "Save word to memory.",
    OPData 0x55 MSTORE8 2 0 "Save byte to memory.",
    OPData 0x56 SLOAD 1 1 "Load word from storage.",
    OPData 0x57 SSTORE 2 0 "Save word to storage.",
    OPData 0x58 JUMP 1 0 "Alter the program counter.",
    OPData 0x59 JUMPI 2 0 "Conditionally alter the program counter.",
    OPData 0x5a PC 0 1 "Get the program counter.",
    OPData 0x5b MSIZE 0 1 "Get the size of active memory in bytes.",
    OPData 0x5c GAS 0 1 "Get the amount of available gas.",
    {-OPData 0x60 (pushOp 1) 0 1 "Place 1 byte item on stack.",
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
    OPData 0x7f (pushOp 32) 0 1 "Place 32-byte item on stack.",-}
    OPData 0xf0 CREATE 3 1 "Create a new account with associated code.",
    OPData 0xf1 CALL 7 1 "Message-call into an account.",
    OPData 0xf2 RETURN 2 0 "Halt execution returning output data.",
    OPData 0xff SUICIDE 1 0 "Halt execution and register account for later deletion."
  ]


op2CodeMap::M.Map Operation Word8
op2CodeMap=M.fromList $ (\(OPData code op _ _ _) -> (op, code)) <$> opDatas

code2OpMap::M.Map Word8 Operation
code2OpMap=M.fromList $ (\(OPData opcode op _ _ _) -> (opcode, op)) <$> opDatas

op2OpCode::Operation->[Word8]
op2OpCode (PUSH theList) | length theList <= 32 && length theList >= 1 =
  0x5F + fromIntegral (length theList):theList
op2OpCode (PUSH []) = error $ "PUSH needs at least one word"
op2OpCode (PUSH x) = error $ "PUSH can only take up to 32 words: " ++ show x
op2OpCode op =
  case M.lookup op op2CodeMap of
    Just x -> [x]
    Nothing -> error $ "op is missing in op2CodeMap: " ++ show op

opLen::Operation->Word256
opLen (PUSH x) = 1 + fromIntegral (length x)
opLen _ = 1

opCode2Op::B.ByteString->(Operation, Int)
opCode2Op rom | B.null rom = (STOP, 1) --according to the yellowpaper, should return STOP if outside of the code bytestring
opCode2Op rom =
  let opcode = B.head rom in
  if opcode >= 0x60 && opcode <= 0x7f
  then (PUSH $ B.unpack $ B.take (fromIntegral $ opcode-0x5F) $ B.tail rom, fromIntegral $ opcode - 0x5E)
  else
    let op = fromMaybe (error $ "code is missing in code2OpMap: " ++ show (B.head rom)) 
             $ M.lookup opcode code2OpMap in
    (op, 1)


