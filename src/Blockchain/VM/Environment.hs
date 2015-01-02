
module Blockchain.VM.Environment where

import qualified Data.ByteString as B

import Blockchain.Data.Address
import Blockchain.Data.Block
import Blockchain.VM.Code

data Environment =
    Environment {
      envOwner::Address,
      envOrigin::Address,
      envGasPrice::Integer,
      envInputData::B.ByteString,
      envSender::Address,
      envValue::Integer,
      envCode::Code,
      envBlock::Block
    }

