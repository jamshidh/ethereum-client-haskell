
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

import Colors
import Data.RLP
import DB.EthDB
import SHA

class Format a where
  format::a->String
  

instance Format B.ByteString where
  format x = BC.unpack (B16.encode x)

instance Format SHA where
    format (SHA x) = yellow $ padZeros 64 $ showHex x ""

instance Format SHAPtr where
    format (SHAPtr x) = yellow $ format x
    
    

formatNibble::N.Nibble->String
formatNibble x | x > 0xF = error "format called for nibble greater than 0xF"
formatNibble x = showHex x ""

instance Format N.NibbleString where
  format (N.EvenNibbleString s) = blue $ format s
  format (N.OddNibbleString c s) = blue $ formatNibble c ++ format s

instance Format RLPObject where
  format (RLPArray objects) = "[" ++ intercalate ", " (format <$> objects) ++ "]"
  format (RLPScalar n) = "0x" ++ showHex n ""
  format (RLPString s) = "0x" ++ (BC.unpack $ B16.encode $ BC.pack s)


                                                      

instance Format PairOrPtr where
  format (APtr x) = "Ptr: " ++ format x
  format (APair key val) = "Pair: " ++ format key ++ ": " ++ format val

formatVal::Maybe RLPObject->String
formatVal Nothing = red "NULL"
formatVal (Just x) = green (format x)
                     
instance Format NodeData where
  format EmptyNodeData = "    <EMPTY>"
  format (ShortcutNodeData s (Left p)) = "    " ++ format s ++ " -> " ++ format p
  format (ShortcutNodeData s (Right val)) = "    " ++ format s ++ " -> " ++ green (format val)
  format (FullNodeData cs val) = "    val: " ++ formatVal val ++ "\n        " ++ intercalate "\n        " (showChoice <$> zip ([0..]::[Int]) cs)
    where
      showChoice (v, Just p) = blue (showHex v "") ++ ": " ++ green (format p)
      showChoice (v, Nothing) = blue (showHex v "") ++ ": " ++ red "NULL"


