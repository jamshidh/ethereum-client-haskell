
module SampleTransactions where

import qualified Data.ByteString as B
import Data.Functor
import Data.List

import Data.Address
import Data.Transaction
import Constants
import JCommand
import Util
import VM.Code
import VM.Labels
import VM.Opcodes

import Debug.Trace

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

createInit::[JCommand]->[JCommand]->Code
createInit initFunc contract = trace (intercalate "-" $ show <$> contract) $
                               trace (intercalate "\n    " $ fmap show $ snd $ jcompile $ initFunc ++ [ReturnCode contract]) $ 
  compile $ lcompile $ snd $ jcompile $ initFunc ++ [ReturnCode contract]

createContractTX::Transaction
createContractTX =
  createContract (1000*finney) 1000
  $ createInit []
    [
     PermStorage (Number 0) :=: Input 0
    ]

sendMessageTX::Transaction
sendMessageTX =
  createMessage (1000*finney) 1000 (Address 0x9f840fe058ce3d84e319b8c747accc1e52f69426)
  (B.pack $ word256ToBytes 0x1234)



paymentContract::Transaction
paymentContract =
  createContract (1000*finney) 1000
                     $ createInit
                            [
                             PermStorage Caller :=: Number 1000
                            ]
                           (
                            let
                                toAddr = Input 0
                                fromAddr = Caller
                                val = Input 1
                            in
                              [
                               If (PermVal fromAddr :>=: val) 
                                      [
                                       PermStorage fromAddr :=: PermVal fromAddr :-: val,
                                       PermStorage toAddr :=: PermVal toAddr :+: val
                                      ]
                             
                              ]
                           )

sendCoinTX::Transaction
sendCoinTX =
  createMessage 0 2000 (Address 0x9f840fe058ce3d84e319b8c747accc1e52f69426)
  (B.pack $ word256ToBytes 0x1 ++ word256ToBytes 500)

