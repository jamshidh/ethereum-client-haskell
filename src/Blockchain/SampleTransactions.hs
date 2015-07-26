
module Blockchain.SampleTransactions where

import Prelude  hiding (EQ)

import qualified Data.ByteString as B
import Network.Haskoin.Internals hiding (Address)

import Blockchain.Data.Address
import Blockchain.Data.Code
import Blockchain.Data.Transaction
import Blockchain.Constants
import Blockchain.ExtWord
import Blockchain.JCommand
import Blockchain.VM.Code
import Blockchain.VM.Labels
import Blockchain.VM.Opcodes

--import Debug.Trace

createContract::Monad m=>Integer->Integer->Code->PrvKey->SecretT m Transaction
createContract val gl code prvKey = 
    createContractCreationTX 0 0x9184e72a000 gl val code prvKey

createMessage::Monad m=>Integer->Integer->Address->B.ByteString->PrvKey->SecretT m Transaction
createMessage val gl toAddr theData prvKey = createMessageTX 0 0x9184e72a000 gl toAddr val theData prvKey

----------------------

simpleTX::Monad m=>PrvKey->SecretT m Transaction
simpleTX =
  createContract 0 550
  $ compile
    [
      PUSH [2],
      PUSH [0],
      MSTORE,
      PUSH [0x20],
      PUSH [0],
      RETURN
    ]

outOfGasTX::Monad m=>PrvKey->SecretT m Transaction
outOfGasTX =
  createContract 3 522
  $ compile
    [
      PUSH [1],
      PUSH [0],
      MSTORE
    ]

simpleStorageTX::Monad m=>PrvKey->SecretT m Transaction
simpleStorageTX =
  createContract 3 1000
  $ compile 
    [
      PUSH [1],
      PUSH [0],
      SSTORE
    ]

createInit::[JCommand]->[JCommand]->Code
createInit initFunc contract = -- trace (intercalate "-" $ show <$> contract) $
--                               trace (intercalate "\n    " $ fmap show $ snd $ jcompile $ initFunc ++ [ReturnCode contract]) $  do
  compile $ lcompile $ snd $ jcompile $ initFunc ++ [ReturnCode $ compile $ lcompile $ snd $ jcompile contract]

createContractTX::Monad m=>PrvKey->SecretT m Transaction
createContractTX =
  createContract (1000*finney) 1000
  $ createInit []
    [
     PermStorage (Number 0) :=: Input 0
    ]

sendMessageTX::Monad m=>PrvKey->SecretT m Transaction
sendMessageTX =
  createMessage (1000*finney) 1000 (Address 0x9f840fe058ce3d84e319b8c747accc1e52f69426)
                    (B.pack $ word256ToBytes 0x1234)



paymentContract::Monad m=>PrvKey->SecretT m Transaction
paymentContract =
  createContract (1000*finney) 2000
                     $ createInit
                            [
                             PermStorage Caller :=: Number 1000
                            ]
                           (
                            let
                                toAddr = Input (0*32)
                                fromAddr = Caller
                                val = Input (1*32)
                            in
                              [
                               If (PermVal fromAddr :>=: val) 
                                      [
                                       PermStorage fromAddr :=: PermVal fromAddr - val,
                                       PermStorage toAddr :=: PermVal toAddr + val
                                      ]
                             
                              ]
                           )

sendCoinTX::Monad m=>PrvKey->SecretT m Transaction
sendCoinTX =
  createMessage 0 2000 (Address 0x9f840fe058ce3d84e319b8c747accc1e52f69426)
  (B.pack $ word256ToBytes 0x1 ++ word256ToBytes 500)




keyValuePublisher::Monad m=>PrvKey->SecretT m Transaction
keyValuePublisher = 
  createContract (1000*finney) 2000
                     $ createInit
                            [
                             PermStorage 69 :=: Caller
                            ]
                           (
                            let
                                inputP = MemStorage (Number 0)
                                inputPr = MemVal (Number 0)
                            in
                              [
                               If (Caller :==: PermVal (Number 69)) 
                                      [
                                       While (inputPr :<: CallDataSize)
                                                 [
                                                  PermStorage (Input inputPr) :=: Input (inputPr + 32),
                                                  inputP :=: inputPr + 64
                                                 ]
                                      ]
                             
                              ]
                           )


sendKeyVal::Monad m=>PrvKey->SecretT m Transaction
sendKeyVal prvKey =
  createMessage 0 2000 (Address 0x9f840fe058ce3d84e319b8c747accc1e52f69426)
                    (B.pack $ word256ToBytes 1000 ++ word256ToBytes 2000 ++ word256ToBytes 1234 ++ word256ToBytes 1)
                    prvKey



{-

    (when (= (caller) @@69)
      (for {} (< @i (calldatasize)) [i](+ @i 64)
        [[ (calldataload @i) ]] (calldataload (+ @i 32))
      )
-}



mysteryCode::[Operation]
mysteryCode =
  [
    PUSH [3,144],
    DUP1,
    PUSH [0,14],
    PUSH [0],
    CODECOPY,
    PUSH [3,158],
    JUMP,
    PUSH [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    PUSH [32],
    MSTORE,
    PUSH [0],
    PUSH [64],
    MSTORE,
    PUSH [1],
    PUSH [96],
    MSTORE,
    PUSH [2],
    PUSH [128],
    MSTORE,
    PUSH [3],
    PUSH [160],
    MSTORE,
    PUSH [0],
    PUSH [192],
    MSTORE,
    PUSH [1],
    PUSH [224],
    MSTORE,
    PUSH [2],
    PUSH [1,0],
    MSTORE,
    PUSH [0],
    PUSH [1,32],
    MSTORE,
    PUSH [1],
    PUSH [1,64],
    MSTORE,
    PUSH [2],
    PUSH [1,96],
    MSTORE,
    PUSH [3],
    PUSH [1,128],
    MSTORE,
    PUSH [3],
    PUSH [1,160],
    MSTORE,
    PUSH [32],
    CALLDATASIZE,
    DIV,
    PUSH [1,192],
    MSTORE,
    PUSH [1,160],
    MLOAD,
    PUSH [1,128],
    MLOAD,
    ADD,
    PUSH [1,192],
    MLOAD,
    SLT,
    ISZERO,
    PUSH [0,136],
    JUMPI,
    PUSH [1,64],
    MLOAD,
    PUSH [1,224],
    MSTORE,
    PUSH [32],
    PUSH [1,224],
    CALLCODE,
    JUMPDEST,
    PUSH [0],
    PUSH [1,128],
    MLOAD,
    PUSH [1,160],
    MLOAD,
    PUSH [1,192],
    MLOAD,
    SUB,
    SMOD,
    EQ,
    ISZERO,
    ISZERO,
    PUSH [0,174],
    JUMPI,
    PUSH [1,64],
    MLOAD,
    PUSH [2,0],
    MSTORE,
    PUSH [32],
    PUSH [2,0],
    CALLCODE,
    JUMPDEST,
    PUSH [1,128],
    MLOAD,
    PUSH [1,160],
    MLOAD,
    PUSH [1,192],
    MLOAD,
    SUB,
    SDIV,
    PUSH [2,32],
    MSTORE,
    PUSH [0],
    PUSH [2,64],
    MSTORE,
    PUSH [2],
    PUSH [1,160],
    MLOAD,
    ADD,
    PUSH [32],
    MUL,
    CALLDATALOAD,
    PUSH [2,96],
    MSTORE,
    PUSH [0],
    PUSH [2,128],
    MSTORE,
    JUMPDEST,
    PUSH [2,32],
    MLOAD,
    PUSH [2,64],
    MLOAD,
    SLT,
    ISZERO,
    PUSH [1,155],
    JUMPI,
    PUSH [1],
    PUSH [1,160],
    MLOAD,
    PUSH [1,128],
    MLOAD,
    PUSH [2,64],
    MLOAD,
    MUL,
    ADD,
    ADD,
    PUSH [32],
    MUL,
    CALLDATALOAD,
    PUSH [2,160],
    MSTORE,
    PUSH [2],
    PUSH [1,160],
    MLOAD,
    PUSH [1,128],
    MLOAD,
    PUSH [2,64],
    MLOAD,
    MUL,
    ADD,
    ADD,
    PUSH [32],
    MUL,
    CALLDATALOAD,
    PUSH [2,192],
    MSTORE,
    PUSH [2,96],
    MLOAD,
    PUSH [2,192],
    MLOAD,
    EQ,
    ISZERO,
    ISZERO,
    PUSH [1,80],
    JUMPI,
    PUSH [2,192],
    MLOAD,
    PUSH [2,96],
    MSTORE,
    PUSH [0],
    PUSH [2,128],
    MLOAD,
    EQ,
    ISZERO,
    ISZERO,
    PUSH [1,79],
    JUMPI,
    PUSH [1,96],
    MLOAD,
    PUSH [2,224],
    MSTORE,
    PUSH [32],
    PUSH [2,224],
    CALLCODE,
    JUMPDEST,
    JUMPDEST,
    PUSH [2,160],
    MLOAD,
    PUSH [2,128],
    MLOAD,
    ADD,
    PUSH [2,128],
    MSTORE,
    PUSH [1],
    PUSH [2,32],
    MLOAD,
    SUB,
    PUSH [2,64],
    MLOAD,
    EQ,
    ISZERO,
    PUSH [1,139],
    JUMPI,
    PUSH [0],
    PUSH [2,128],
    MLOAD,
    EQ,
    ISZERO,
    ISZERO,
    PUSH [1,138],
    JUMPI,
    PUSH [1,96],
    MLOAD,
    PUSH [3,0],
    MSTORE,
    PUSH [32],
    PUSH [3,0],
    CALLCODE,
    JUMPDEST,
    JUMPDEST,
    PUSH [1],
    PUSH [2,64],
    MLOAD,
    ADD,
    PUSH [2,64],
    MSTORE,
    PUSH [0,220],
    JUMP,
    JUMPDEST,
    PUSH [32],
    MLOAD,
    SLOAD,
    PUSH [3,32],
    MSTORE,
    PUSH [1],
    PUSH [32],
    MLOAD,
    SLOAD,
    ADD,
    PUSH [32],
    MLOAD,
    SSTORE,
    PUSH [32],
    CALLDATALOAD,
    PUSH [3,64],
    MSTORE,
    PUSH [64],
    CALLDATALOAD,
    PUSH [3,96],
    MSTORE,
    PUSH [255,255,255,255,255,255,255,255],
    PUSH [3,128],
    MSTORE,
    PUSH [3,64],
    MLOAD,
    PUSH [64],
    MLOAD,
    PUSH [1,0,0,0,0,0,0,0,0],
    PUSH [3,128],
    MLOAD,
    MUL,
    PUSH [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    PUSH [3,32],
    MLOAD,
    MUL,
    ADD,
    ADD,
    SSTORE,
    PUSH [3,96],
    MLOAD,
    PUSH [96],
    MLOAD,
    PUSH [1,0,0,0,0,0,0,0,0],
    PUSH [3,128],
    MLOAD,
    MUL,
    PUSH [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    PUSH [3,32],
    MLOAD,
    MUL,
    ADD,
    ADD,
    SSTORE,
    NUMBER,
    PUSH [128],
    MLOAD,
    PUSH [1,0,0,0,0,0,0,0,0],
    PUSH [3,128],
    MLOAD,
    MUL,
    PUSH [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    PUSH [3,32],
    MLOAD,
    MUL,
    ADD,
    ADD,
    SSTORE,
    TIMESTAMP,
    PUSH [160],
    MLOAD,
    PUSH [1,0,0,0,0,0,0,0,0],
    PUSH [3,128],
    MLOAD,
    MUL,
    PUSH [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    PUSH [3,32],
    MLOAD,
    MUL,
    ADD,
    ADD,
    SSTORE,
    PUSH [0],
    PUSH [2,64],
    MSTORE,
    JUMPDEST,
    PUSH [2,32],
    MLOAD,
    PUSH [2,64],
    MLOAD,
    SLT,
    ISZERO,
    PUSH [3,129],
    JUMPI,
    PUSH [1,160],
    MLOAD,
    PUSH [1,128],
    MLOAD,
    PUSH [2,64],
    MLOAD,
    MUL,
    ADD,
    PUSH [32],
    MUL,
    CALLDATALOAD,
    PUSH [3,160],
    MSTORE,
    PUSH [1],
    PUSH [1,160],
    MLOAD,
    PUSH [1,128],
    MLOAD,
    PUSH [2,64],
    MLOAD,
    MUL,
    ADD,
    ADD,
    PUSH [32],
    MUL,
    CALLDATALOAD,
    PUSH [2,160],
    MSTORE,
    PUSH [2],
    PUSH [1,160],
    MLOAD,
    PUSH [1,128],
    MLOAD,
    PUSH [2,64],
    MLOAD,
    MUL,
    ADD,
    ADD,
    PUSH [32],
    MUL,
    CALLDATALOAD,
    PUSH [2,192],
    MSTORE,
    PUSH [3,160],
    MLOAD,
    PUSH [192],
    MLOAD,
    PUSH [1,0,0,0,0,0,0,0,0],
    PUSH [2,64],
    MLOAD,
    MUL,
    PUSH [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    PUSH [3,32],
    MLOAD,
    MUL,
    ADD,
    ADD,
    SSTORE,
    PUSH [2,160],
    MLOAD,
    PUSH [224],
    MLOAD,
    PUSH [1,0,0,0,0,0,0,0,0],
    PUSH [2,64],
    MLOAD,
    MUL,
    PUSH [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    PUSH [3,32],
    MLOAD,
    MUL,
    ADD,
    ADD,
    SSTORE,
    PUSH [2,192],
    MLOAD,
    PUSH [1,0],
    MLOAD,
    PUSH [1,0,0,0,0,0,0,0,0],
    PUSH [2,64],
    MLOAD,
    MUL,
    PUSH [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    PUSH [3,32],
    MLOAD,
    MUL,
    ADD,
    ADD,
    SSTORE,
    PUSH [1],
    PUSH [2,64],
    MLOAD,
    ADD,
    PUSH [2,64],
    MSTORE,
    PUSH [2,138],
    JUMP,
    JUMPDEST,
    PUSH [1,32],
    MLOAD,
    PUSH [3,192],
    MSTORE,
    PUSH [32],
    PUSH [3,192],
    CALLCODE,
    JUMPDEST,
    PUSH [0],
    CALLCODE
  ]

createMysteryContract::Monad m=>PrvKey->SecretT m Transaction
createMysteryContract prvKey = 
    createContractCreationTX  0 0x9184e72a000 8000 0 (compile mysteryCode) prvKey
