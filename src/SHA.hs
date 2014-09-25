
module SHA (
  SHA(..),
  rlp2SHA,
  sha2RLP
  ) where

import Data.Binary
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Network.Haskoin.Crypto
import Numeric

import Format
import RLP

newtype SHA = SHA Word256 deriving (Show)

padZeros::Int->String->String
padZeros n s = replicate (n - length s) '0' ++ s

instance Format SHA where
  format (SHA x) = padZeros 32 $ showHex x ""

rlp2SHA::RLPObject->SHA
rlp2SHA (RLPString s) | length s == 32 = SHA $ decode $ BLC.pack s

sha2RLP::SHA->RLPObject
sha2RLP (SHA val) = RLPString $ BLC.unpack $ encode val
