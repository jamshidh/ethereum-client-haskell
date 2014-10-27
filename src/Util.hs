
module Util (
  byteString2Integer,
  integer2Bytes,
  word160ToBytes,
  word256ToBytes,
  padZeros,
  tab
  ) where

import Data.Bits
import qualified Data.ByteString as B
import Data.Word
import Network.Haskoin.Crypto (Word160, Word256)

--I hate this, it is an ugly way to create an Integer from its component bytes.
--There should be an easier way....
--See http://stackoverflow.com/questions/25854311/efficient-packing-bytes-into-integers
byteString2Integer::B.ByteString->Integer
byteString2Integer x = byteString2Integer' $ B.unpack x
  where
    byteString2Integer'::[Word8]->Integer
    byteString2Integer' [] = 0
    byteString2Integer' (byte:rest) = fromIntegral byte `shift` (8 * length rest) + byteString2Integer' rest

integer2Bytes::Integer->[Word8]
integer2Bytes 0 = []
integer2Bytes x = integer2Bytes (x `shiftR` 8) ++ [fromInteger (x .&. 255)]

word256ToBytes::Word256->[Word8]
word256ToBytes x =
     map (\byte -> fromIntegral $ (x `shiftR` (byte*8)) .&. 0xFF) $ [31,30..0]

word160ToBytes::Word160->[Word8]
word160ToBytes x =
     map (\byte -> fromIntegral $ (x `shiftR` (byte*8)) .&. 0xFF) $ [19,18..0]

padZeros::Int->String->String
padZeros n s = replicate (n - length s) '0' ++ s

tab::String->String
tab [] = []
tab ('\n':rest) = '\n':' ':' ':' ':' ':tab rest
tab (c:rest) = c:tab rest
