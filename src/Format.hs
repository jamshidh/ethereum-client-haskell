
module Format (
  Format(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import Numeric

import qualified Colors as C
import SHA

class Format a where
  format::a->String
  

instance Format B.ByteString where
  format x = BC.unpack (B16.encode x)

instance Format SHA where
    format (SHA x) = C.yellow $ padZeros 64 $ showHex x ""

