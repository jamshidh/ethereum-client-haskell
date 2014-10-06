
module ByteStringTools (
) where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Internal
import Data.Functor
import Data.Word

import Format
import RLP

instance Format B.ByteString where
  format x = BC.unpack (B16.encode x)


instance RLPSerializable B.ByteString where
    rlpEncode s = error "rlpDecode for ByteString not defined"
    rlpDecode (RLPString s) = BC.pack s
    rlpDecode x = error ("rlpDecode for ByteString not defined for: " ++ show x)
