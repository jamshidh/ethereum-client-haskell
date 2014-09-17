
module PrettyBytes (
  ) where

import qualified Data.ByteString as B
import Data.List
import Numeric

import Format

showPaddedHex x | x < 16 = "0" ++ showHex x ""
showPaddedHex x = showHex x ""

instance Format B.ByteString where
--  format = concat . map showPaddedHex . B.unpack
  format = intercalate "-" . map showPaddedHex . B.unpack
  
