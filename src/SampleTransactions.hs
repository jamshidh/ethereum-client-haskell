
module SampleTransactions where

import qualified Data.ByteString as B

import Address
import Code
import Transaction
import Util

simpleTX::Transaction
simpleTX =
    --runResourceT $ do
      --open state
      Transaction {
                     tNonce = 28,
                     gasPrice = 0x9184e72a000,
                     tGasLimit = 550,
                     to = Address 0, --0x5b42bd01ff7b368cd80a477cb1cf0d407e2b1cbe,
                     value = 3,
                     tInit = Code $ B.pack $ integer2Bytes 0x600260005460206000f2
                   }

