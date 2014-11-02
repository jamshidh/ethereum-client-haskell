
module Environment where

import qualified Data.ByteString as B

import Address
import Block

data Environment =
    Environment {
      envOrigin::Address,
      envCaller::Address,
      envGasPrice::Integer,
      envInputData::B.ByteString,
      envSender::Address,
      envValue::Integer,
      envCode::B.ByteString,
      envBlock::Block
    }

