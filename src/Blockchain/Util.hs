
module Blockchain.Util (
  byteString2Integer,
  bytes2Integer,
  integer2Bytes,
  integer2Bytes1,
  word160ToBytes,
  word256ToBytes,
  padZeros,
  tab,
  showMem,
  safeDrop,
  safeTake
  ) where

import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString.Internal
import Data.Functor
import Data.List
import Data.Word
import Network.Haskoin.Crypto (Word160, Word256)
import Numeric

--I hate this, it is an ugly way to create an Integer from its component bytes.
--There should be an easier way....
--See http://stackoverflow.com/questions/25854311/efficient-packing-bytes-into-integers
byteString2Integer::B.ByteString->Integer
byteString2Integer x = bytes2Integer $ B.unpack x

bytes2Integer::[Word8]->Integer
bytes2Integer [] = 0
bytes2Integer (byte:rest) = fromIntegral byte `shift` (8 * length rest) + bytes2Integer rest

integer2Bytes::Integer->[Word8]
integer2Bytes 0 = []
integer2Bytes x = integer2Bytes (x `shiftR` 8) ++ [fromInteger (x .&. 255)]

--integer2Bytes1 is integer2Bytes, but with the extra condition that the output be of length 1 or more.
integer2Bytes1::Integer->[Word8]
integer2Bytes1 0 = [0]
integer2Bytes1 x = integer2Bytes x

word256ToBytes::Word256->[Word8]
word256ToBytes x =
     map (\byte -> fromIntegral $ (x `shiftR` (byte*8)) .&. 0xFF) [31,30..0]

word160ToBytes::Word160->[Word8]
word160ToBytes x =
     map (\byte -> fromIntegral $ (x `shiftR` (byte*8)) .&. 0xFF) [19,18..0]

padZeros::Int->String->String
padZeros n s = replicate (n - length s) '0' ++ s

tab::String->String
tab [] = []
tab ('\n':rest) = '\n':' ':' ':' ':' ':tab rest
tab (c:rest) = c:tab rest

showWord8::Word8->Char
showWord8 c | c >= 32 && c < 128 = w2c c
showWord8 _ = '?'

showMem::Int->[Word8]->String
showMem _ [] = "" 
showMem p (v1:v2:v3:v4:v5:v6:v7:v8:rest) = 
    padZeros 4 (showHex p "") ++ " " 
             ++ [showWord8 v1] ++ [showWord8 v2] ++ [showWord8 v3] ++ [showWord8 v4]
             ++ [showWord8 v5] ++ [showWord8 v6] ++ [showWord8 v7] ++ [showWord8 v8] ++ " "
             ++ padZeros 2 (showHex v1 "") ++ " " ++ padZeros 2 (showHex v2 "") ++ " " ++ padZeros 2 (showHex v3 "") ++ " " ++ padZeros 2 (showHex v4 "") ++ " "
             ++ padZeros 2 (showHex v5 "") ++ " " ++ padZeros 2 (showHex v6 "") ++ " " ++ padZeros 2 (showHex v7 "") ++ " " ++ padZeros 2 (showHex v8 "") ++ "\n"
             ++ showMem (p+8) rest
showMem p x = padZeros 4 (showHex p "") ++ " " ++ (showWord8 <$> x) ++ " " ++ intercalate " " (padZeros 2 <$> flip showHex "" <$> x)


safeTake::Word256->B.ByteString->B.ByteString
safeTake i _ | i > 0x7fffffffffffffff = error "error in call to safeTake: string too long"
safeTake i s | i > fromIntegral (B.length s) = s `B.append` B.replicate (fromIntegral i - B.length s) 0
safeTake i s = B.take (fromIntegral i) s

safeDrop::Word256->B.ByteString->B.ByteString
safeDrop i s | i > fromIntegral (B.length s) = B.empty
safeDrop i _ | i > 0x7fffffffffffffff = error "error in call to safeDrop: string too long"
safeDrop i s = B.drop (fromIntegral i) s

