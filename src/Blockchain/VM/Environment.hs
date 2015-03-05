
module Blockchain.VM.Environment where

import qualified Data.ByteString as B

import Blockchain.Data.Address
import Blockchain.Data.Block
import Blockchain.Data.Code
import Blockchain.ExtWord

data Environment =
    Environment {
      envOwner::Address,
      envOrigin::Address,
      envGasPrice::Integer,
      envInputData::B.ByteString,
      envSender::Address,
      envValue::Integer,
      envCode::Code,
      envJumpDests::[Word256],
      envBlock::Block
    }

