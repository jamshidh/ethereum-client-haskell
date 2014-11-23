
module Format (
  Format(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import Data.Functor
import Data.List
import qualified Data.NibbleString as N
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Colors as C
import Data.RLP
import Database.MerklePatricia
import SHA

class Format a where
  format::a->String
  

instance Format B.ByteString where
  format x = BC.unpack (B16.encode x)

instance Format SHA where
    format (SHA x) = C.yellow $ padZeros 64 $ showHex x ""

instance Format SHAPtr where
    format (SHAPtr x) = C.yellow $ format x
    
    

formatNibble::N.Nibble->String
formatNibble x | x > 0xF = error "format called for nibble greater than 0xF"
formatNibble x = showHex x ""

instance Format N.NibbleString where
  format (N.EvenNibbleString s) = C.blue $ format s
  format (N.OddNibbleString c s) = C.blue $ formatNibble c ++ format s
