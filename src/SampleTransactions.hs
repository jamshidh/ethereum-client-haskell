
module SampleTransactions where

import qualified Data.ByteString as B

import Address
import Code
import Constants
import Opcodes
import Transaction
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
              PUSH [5],
              PUSH [12],
              PUSH [0],
              CODECOPY,
              PUSH [5],
              PUSH [0],
              RETURN,
              CALLDATALOAD,
              PUSH [0],
              SSTORE,
              RETURN
            ]
    }

sendMessageTX::Transaction
sendMessageTX =
  MessageTX {
    tNonce = 28,
    gasPrice = 0x9184e72a000,
    tGasLimit = 1000,
    to = Address 0x35de23aac6469dc7fafd36a4d49186ea7e216baf,
    value = 1000*finney,
    tData = B.pack $ word256ToBytes 0x1234
    }

