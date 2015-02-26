
module Blockchain.Data.Log (
  Log(..)
  ) where

import Blockchain.Data.Address
import qualified Data.ByteString as B
import Network.Haskoin.Internals (Word256, Word512)

data Log =
  Log {
    address::Address,
    bloom::Word512,
    logData::B.ByteString,
    topics::[Word256]
    } deriving (Show, Eq)
