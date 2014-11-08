
module VM.Environment where

import qualified Data.ByteString as B

import Data.Address
import Data.Block
import VM.Code

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

