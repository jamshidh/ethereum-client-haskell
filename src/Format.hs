
module Format (
  Format(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import qualified Data.NibbleString as N
import Numeric

import Colors

class Format a where
  format::a->String
  

instance Format B.ByteString where
  format x = BC.unpack (B16.encode x)

formatNibble::N.Nibble->String
formatNibble x | x > 0xF = error "format called for nibble greater than 0xF"
formatNibble x = showHex x ""

instance Format N.NibbleString where
  format (N.EvenNibbleString s) = blue $ format s
  format (N.OddNibbleString c s) = blue $ formatNibble c ++ format s

