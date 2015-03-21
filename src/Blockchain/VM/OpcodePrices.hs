{-# OPTIONS_GHC  -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module Blockchain.VM.OpcodePrices where

import Prelude hiding (EQ, LT, GT)

import Blockchain.ExtWord
import Blockchain.VM.Opcodes

opGasPrice::Operation->Integer
opGasPrice DUP1 =	3
opGasPrice DUP2 =	3
opGasPrice DUP3 =	3
opGasPrice DUP4 =	3
opGasPrice DUP5 =	3
opGasPrice DUP6 =	3
opGasPrice DUP7 =	3
opGasPrice DUP8 =	3
opGasPrice DUP9 =	3
opGasPrice DUP10 =	3
opGasPrice DUP11 =	3
opGasPrice DUP12 =	3
opGasPrice DUP13 =	3
opGasPrice DUP14 =	3
opGasPrice DUP15 =	3
opGasPrice DUP16 =	3
opGasPrice SWAP1 =	3
opGasPrice SWAP2 =	3
opGasPrice SWAP3 =	3
opGasPrice SWAP4 =	3
opGasPrice SWAP5 =	3
opGasPrice SWAP6 =	3
opGasPrice SWAP7 =	3
opGasPrice SWAP8 =	3
opGasPrice SWAP9 =	3
opGasPrice SWAP10 =	3
opGasPrice SWAP11 =	3
opGasPrice SWAP12 =	3
opGasPrice SWAP13 =	3
opGasPrice SWAP14 =	3
opGasPrice SWAP15 =	3
opGasPrice SWAP16 =	3
opGasPrice (PUSH _) =	3
opGasPrice ADD =	3
opGasPrice MUL =	5
opGasPrice SUB =	3
opGasPrice DIV =	5
opGasPrice SDIV =	5
opGasPrice MOD =	5
opGasPrice SMOD =	5
opGasPrice ADDMOD =	8
opGasPrice MULMOD =	8
opGasPrice SIGNEXTEND =	5
opGasPrice LT =	3
opGasPrice GT =	3
opGasPrice SLT =	3
opGasPrice SGT =	3
opGasPrice EQ =	3
opGasPrice ISZERO =	3
opGasPrice AND =	3
opGasPrice OR =	3
opGasPrice XOR =	3
opGasPrice NOT =	3
opGasPrice BYTE =	3
opGasPrice ADDRESS =	2
opGasPrice BALANCE =	20
opGasPrice ORIGIN =	2
opGasPrice CALLER =	2
opGasPrice CALLVALUE =	2
opGasPrice CALLDATALOAD =	3
opGasPrice CALLDATASIZE =	2
opGasPrice CODESIZE =	2
opGasPrice GASPRICE =	2
opGasPrice EXTCODESIZE =	20
opGasPrice BLOCKHASH =	20
opGasPrice COINBASE =	2
opGasPrice TIMESTAMP =	2
opGasPrice NUMBER =	2
opGasPrice DIFFICULTY =	2
opGasPrice GASLIMIT =	2
opGasPrice POP =	2
opGasPrice MLOAD =	3
opGasPrice MSTORE =	3
opGasPrice MSTORE8 =	3
opGasPrice SLOAD =	50
opGasPrice JUMP =	8
opGasPrice JUMPI =	10
opGasPrice PC =	2
opGasPrice MSIZE =	2
opGasPrice GAS =	2
opGasPrice JUMPDEST =	1
opGasPrice CREATE =	32000
opGasPrice CALLCODE =	40
opGasPrice RETURN =	0
opGasPrice STOP =	0
opGasPrice SUICIDE =	0

opGasPrice (MalformedOpcode _) = 0 --gonna fail anyway, just put something arbitrary here

opGasPrice x = error $ "Missing opcode in opCodePrice: " ++ show x






gTX =	21000
gTXDATANONZERO =	68
gTXDATAZERO = 4


gMEMWORD =	3
gQUADCOEFFDIV = 512


gEXPBASE =	10
gEXPBYTE =	10

gCALLDATACOPYBASE =	3

gCODECOPYBASE =	3
gEXTCODECOPYBASE =	20
gCOPYWORD =	3


gLOG =	375
gLOGTOPIC =	375
gLOGDATA =	8

gCALL =	40
gCALLVALUETRANSFER = 9000::Word256
gCALLSTIPEND = 2300::Word256
gCALLNEWACCOUNT = 25000::Word256

gCREATEDATA =	200



gSHA3BASE =	30
gSHA3WORD =	6
gECRECOVER =	3000
gSHA256BASE =	60
gSHA256WORD =	12
gRIPEMD160BASE =	600
gRIPEMD160WORD =	120
gIDENTITYBASE =	15
gIDENTITYWORD =	3
