
module VM where

data Operation = STOP | ADD | MUL | SUB | DIV | SDIV | MOD | SMOD | EXP | NEG | LT | GT | SLT | SGT | EQ | NOT | AND | OR | XOR | BYTE | SHA3 | ADDRESS | BALANCE | ORIGIN | CALLER | CALLVALUE | CALLDATALOAD | CALLDATASIZE | CALLDATACOPY | CODESIZE | CODECOPY | GASPRICE | PREVHASH | COINBASE | TIMESTAMP | NUMBER | DIFFICULTY | GASLIMIT | POP | DUP | SWAP | MLOAD | MSTORE | MSTORE8 | SLOAD | SSTORE | JUMP | JUMPI | PC | MSIZE | GAS | PUSH1 | PUSH2 | PUSH3 | PUSH4 | PUSH5 | PUSH6 | PUSH7 | PUSH8 | PUSH9 | PUSH10 | PUSH11 | PUSH12 | PUSH13 | PUSH14 | PUSH15 | PUSH16 | PUSH17 | PUSH18 | PUSH19 | PUSH20 | PUSH21 | PUSH22 | PUSH23 | PUSH24 | PUSH25 | PUSH26 | PUSH27 | PUSH28 | PUSH29 | PUSH30 | PUSH31 | PUSH32 | CREATE | CALL | RETURN | SUICIDE

0x00 STOP
0x01 ADD
0x02 MUL
0x03 SUB
0x04 DIV
0x05 SDIV
0x06 MOD
0x07 SMOD
0x08 EXP
0x09 NEG
0x0a LT
0x0b GT
0x0c SLT
0x0d SGT
0x0e EQ
0x0f NOT
0x10 AND
0x11 OR
0x12 XOR
0x13 BYTE
0x20 SHA3
0x30 ADDRESS
0x31 BALANCE
0x32 ORIGIN
0x33 CALLER
0x34 CALLVALUE
0x35 CALLDATALOAD
0x36 CALLDATASIZE
0x37 CALLDATACOPY
0x38 CODESIZE
0x39 CODECOPY
0x3a GASPRICE
0x40 PREVHASH
0x41 COINBASE
0x42 TIMESTAMP
0x43 NUMBER
0x44 DIFFICULTY
0x45 GASLIMIT
0x50 POP
0x51 DUP
0x52 SWAP
0x53 MLOAD
0x54 MSTORE
0x55 MSTORE8
0x56 SLOAD
0x57 SSTORE
0x58 JUMP
0x59 JUMPI
0x5a PC
0x5b MSIZE
0x5c GAS
0x60 PUSH1
0x61 PUSH2
0x62 PUSH3
0x63 PUSH4
0x64 PUSH5
0x65 PUSH6
0x66 PUSH7
0x67 PUSH8
0x68 PUSH9
0x69 PUSH10
0x6a PUSH11
0x6b PUSH12
0x6c PUSH13
0x6d PUSH14
0x6e PUSH15
0x6f PUSH16
0x70 PUSH17
0x71 PUSH18
0x72 PUSH19
0x73 PUSH20
0x74 PUSH21
0x75 PUSH22
0x76 PUSH23
0x77 PUSH24
0x78 PUSH25
0x79 PUSH26
0x7a PUSH27
0x7b PUSH28
0x7c PUSH29
0x7d PUSH30
0x7e PUSH31
0x7f PUSH32
0xf0 CREATE
0xf1 CALL
0xf2 RETURN
0xff SUICIDE







instance Binary Operation where
  get = do
    

0x00 STOP 0 0 "Halts execution."
0x01 ADD 2 1 "Addition operation."
0x02 MUL 2 1 "Multiplication operation."
0x03 SUB 2 1 "Subtraction operation."
0x04 DIV 2 1 "Integer division operation."
0x05 SDIV 2 1 "Signed integer division operation."
0x06 MOD 2 1 "Modulo remainder operation."
0x07 SMOD 2 1 "Signed modulo remainder operation."
0x08 EXP 2 1 "Exponential operation."
0x09 NEG 1 1 "Negation operation."
0x0a LT 2 1 "Less-than comparision."
0x0b GT 2 1 "Greater-than comparision."
0x0c SLT 2 1 "Signed less-than comparision."
0x0d SGT 2 1 "Signed greater-than comparision."
0x0e EQ 2 1 "Equality comparision."
0x0f NOT 1 1 "Simple not operator."
0x10 AND 2 1 "Bitwise AND operation."
0x11 OR 2 1 "Bitwise OR operation."
0x12 XOR 2 1 "Bitwise XOR operation."
0x13 BYTE 2 1 "Retrieve single byte from word."
0x20 SHA3 2 1 "Compute SHA3-256 hash."
0x30 ADDRESS 0 1 "Get address of currently executing account."
0x31 BALANCE 1 1 "Get balance of the given account."
0x32 ORIGIN 0 1 "Get execution origination address."
0x33 CALLER 0 1 "Get caller address."
0x34 CALLVALUE 0 1 "Get deposited value by the instruction/transaction responsible for this execution."
0x35 CALLDATALOAD 1 1 "Get input data of current environment."
0x36 CALLDATASIZE 0 1 "Get size of input data in current environment."
0x37 CALLDATACOPY 3 0 "Copy input data in current environment to memory."
0x38 CODESIZE 0 1 "Get size of code running in current environment."
0x39 CODECOPY 3 0 "Copy code running in current environment to memory."
0x3a GASPRICE 0 1 "Get price of gas in current environment."
0x40 PREVHASH 0 1 "Get hash of most recent complete block."
0x41 COINBASE 0 1 "Get the block’s coinbase address."
0x42 TIMESTAMP 0 1 "Get the block’s timestamp."
0x43 NUMBER 0 1 "Get the block’s number."
0x44 DIFFICULTY 0 1 "Get the block’s difficulty."
0x45 GASLIMIT 0 1 "Get the block’s gas limit."
0x50 POP 1 0 "Remove item from stack."
0x51 DUP 1 2 "Duplicate stack item."
0x52 SWAP 2 2 "Exchange stack items."
0x53 MLOAD 1 1 "Load word from memory."
0x54 MSTORE 2 0 "Save word to memory."
0x55 MSTORE8 2 0 "Save byte to memory."
0x56 SLOAD 1 1 "Load word from storage."
0x57 SSTORE 2 0 "Save word to storage."
0x58 JUMP 1 0 "Alter the program counter."
0x59 JUMPI 2 0 "Conditionally alter the program counter."
0x5a PC 0 1 "Get the program counter."
0x5b MSIZE 0 1 "Get the size of active memory in bytes."
0x5c GAS 0 1 "Get the amount of available gas."
0x60 PUSH1 0 1 "Place 1 byte item on stack".
0x61 PUSH2 0 1 "Place 2-byte item on stack."
0x62 PUSH3 0 1 "Place 3-byte item on stack."
0x63 PUSH4 0 1 "Place 4-byte item on stack."
0x64 PUSH5 0 1 "Place 5-byte item on stack."
0x65 PUSH6 0 1 "Place 6-byte item on stack."
0x66 PUSH7 0 1 "Place 7-byte item on stack."
0x67 PUSH8 0 1 "Place 8-byte item on stack."
0x68 PUSH9 0 1 "Place 9-byte item on stack."
0x69 PUSH10 0 1 "Place 10-byte item on stack."
0x6a PUSH11 0 1 "Place 11-byte item on stack."
0x6b PUSH12 0 1 "Place 12-byte item on stack."
0x6c PUSH13 0 1 "Place 13-byte item on stack."
0x6d PUSH14 0 1 "Place 14-byte item on stack."
0x6e PUSH15 0 1 "Place 15-byte item on stack."
0x6f PUSH16 0 1 "Place 16-byte item on stack."
0x70 PUSH17 0 1 "Place 17-byte item on stack."
0x71 PUSH18 0 1 "Place 18-byte item on stack."
0x72 PUSH19 0 1 "Place 19-byte item on stack."
0x73 PUSH20 0 1 "Place 20-byte item on stack."
0x74 PUSH21 0 1 "Place 21-byte item on stack."
0x75 PUSH22 0 1 "Place 22-byte item on stack."
0x76 PUSH23 0 1 "Place 23-byte item on stack."
0x77 PUSH24 0 1 "Place 24-byte item on stack."
0x78 PUSH25 0 1 "Place 25-byte item on stack."
0x79 PUSH26 0 1 "Place 26-byte item on stack."
0x7a PUSH27 0 1 "Place 27-byte item on stack."
0x7b PUSH28 0 1 "Place 28-byte item on stack."
0x7c PUSH29 0 1 "Place 29-byte item on stack."
0x7d PUSH30 0 1 "Place 30-byte item on stack."
0x7e PUSH31 0 1 "Place 31-byte item on stack."
0x7f PUSH32 0 1 "Place 32-byte item on stack."
0xf0 CREATE 3 1 "Create a new account with associated code."
0xf1 CALL 7 1 "Message-call into an account."
0xf2 RETURN 2 0 "Halt execution returning output data."
0xff SUICIDE 1 0 "Halt execution and register account for later deletion."
