
module SampleTransactions where

import qualified Data.ByteString as B

import Data.Address
import VM.Code
import Constants
import VM.Opcodes
import Data.Transaction
import Util

createContract::Integer->Integer->Code->Transaction
createContract val gl code = ContractCreationTX  {
  tNonce = 0,
  gasPrice = 0x9184e72a000,
  tGasLimit = gl,
  value = val,
  tInit = code
  }

createMessage::Integer->Integer->Address->B.ByteString->Transaction
createMessage val gl toAddr theData = MessageTX  {
  tNonce = 0,
  gasPrice = 0x9184e72a000,
  tGasLimit = gl,
  to = toAddr,
  value = val,
  tData = theData
  }

----------------------

simpleTX::Transaction
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

outOfGasTX::Transaction
outOfGasTX =
  createContract 3 550
  $ compile
    [
      PUSH [1],
      PUSH [0],
      MSTORE
    ]

simpleStorageTX::Transaction
simpleStorageTX =
  createContract 3 1000
  $ compile 
    [
      PUSH [1],
      PUSH [0],
      SSTORE
    ]

createInit::[Operation]->[Operation]->Code
createInit init contract =
  Code $ B.pack $ initBytes ++ copyCodeBytes ++ contractBytes
  where
    initBytes = op2OpCode =<< init
    contractBytes = op2OpCode =<< contract
    copyCode =
      [
        PUSH $ integer2Bytes $ toInteger $ length contractBytes,
        PUSH $ integer2Bytes $ toInteger $ length initBytes + length copyCode,
        PUSH [0],
        CODECOPY,
        PUSH $ integer2Bytes $ toInteger $ length contractBytes,
        PUSH [0],
        RETURN
      ]
    copyCodeBytes = op2OpCode =<< copyCode

createContractTX::Transaction
createContractTX =
  createContract (1000*finney) 1000
  $ createInit []
    [
      PUSH [0],
      CALLDATALOAD,
      PUSH [0],
      SSTORE,
      STOP
    ]

sendMessageTX::Transaction
sendMessageTX =
  createMessage (1000*finney) 1000 (Address 0x9f840fe058ce3d84e319b8c747accc1e52f69426)
  (B.pack $ word256ToBytes 0x1234)



paymentContract::Transaction
paymentContract =
  createContract (1000*finney) 1000
  $ compile 
    [
    
    ]
