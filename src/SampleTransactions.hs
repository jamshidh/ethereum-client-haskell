
module SampleTransactions where

import qualified Data.ByteString as B

import Data.Address
import VM.Code
import Constants
import VM.Opcodes
import Data.Transaction
import Util

simpleTX::Transaction
simpleTX =
  ContractCreationTX {
    tNonce = 28,
    gasPrice = 0x9184e72a000,
    tGasLimit = 550,
    value = 3,
    tInit = Code $ B.pack $ integer2Bytes 0x600260005460206000f2
    }

outOfGasTX::Transaction
outOfGasTX =
  ContractCreationTX {
    tNonce = 28,
    gasPrice = 0x9184e72a000,
    tGasLimit = 550,
    value = 3,
    tInit = Code $ B.pack $ integer2Bytes 0x6001600057
    }

simpleStorageTX::Transaction
simpleStorageTX =
  ContractCreationTX {
    tNonce = 28,
    gasPrice = 0x9184e72a000,
    tGasLimit = 1000,
    value = 3,
    tInit = compile
            [
              PUSH [1],
              PUSH [0],
              SSTORE
            ]
    }


createContractTX::Transaction
createContractTX =
  ContractCreationTX {
    tNonce = 28,
    gasPrice = 0x9184e72a000,
    tGasLimit = 1000,
    value = 1000*finney,
    tInit = compile
            [
              PUSH [7],
              PUSH [12],
              PUSH [0],
              CODECOPY,
              PUSH [7],
              PUSH [0],
              RETURN,
              PUSH [0],
              CALLDATALOAD,
              PUSH [0],
              SSTORE,
              STOP
            ]
    }

sendMessageTX::Transaction
sendMessageTX =
  MessageTX {
    tNonce = 28,
    gasPrice = 0x9184e72a000,
    tGasLimit = 1000,
    to = Address 0x9f840fe058ce3d84e319b8c747accc1e52f69426,
    value = 1000*finney,
    tData = B.pack $ word256ToBytes 0x1234
    }

