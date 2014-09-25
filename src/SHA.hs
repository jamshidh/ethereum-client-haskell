
module SHA (
  SHA(..),
  rlp2SHA,
  sha2RLP,
  rlp2Word512,
  word5122RLP,
  padZeros
  ) where

import Data.Binary
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Network.Haskoin.Internals
import Numeric

import Format
import RLP
import Util

newtype SHA = SHA Word256 deriving (Show)

instance Format SHA where
  format (SHA x) = padZeros 32 $ showHex x ""

rlp2SHA::RLPObject->SHA
rlp2SHA (RLPString s) | length s == 32 = SHA $ decode $ BLC.pack s

sha2RLP::SHA->RLPObject
sha2RLP (SHA val) = RLPString $ BLC.unpack $ encode val

--------------------- Word512 stuff

rlp2Word512::RLPObject->Word512
rlp2Word512 (RLPString s) | length s == 64 = decode $ BLC.pack s
rlp2Word512 x = error ("Missing case in rlp2Word512: " ++ show x)

word5122RLP::Word512->RLPObject
word5122RLP val = RLPString $ BLC.unpack $ encode val

