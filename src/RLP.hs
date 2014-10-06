{-# LANGUAGE FlexibleInstances #-}

module RLP (
  RLPObject(..),
  RLPSerializable(..),
  rlpDeserialize,
  rlp2Bytes,
  rlpNumber,
  getNumber
  ) where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal
import Data.Functor
import Data.List
import Data.Word

import Format
import Util

data RLPObject = RLPNumber Int | RLPString String | RLPArray [RLPObject] deriving (Show)

instance Format RLPObject where
  format (RLPArray objects) = "[" ++ intercalate ", " (format <$> objects) ++ "]"
  format (RLPNumber n) = "0x" ++ show n
  format (RLPString s) = "0x" ++ (BC.unpack $ B16.encode $ BC.pack s)


class RLPSerializable a where
  rlpDecode::RLPObject->a
  rlpEncode::a->RLPObject

splitAtWithError::Int->[a]->([a], [a])
splitAtWithError n arr | n > length arr = error "splitAtWithError called with n > length arr"
splitAtWithError n arr = splitAt n arr

rlpSplit::[Word8]->(RLPObject, [Word8])
rlpSplit (x:rest) | x >= 192 && x < 192+55 =
  (RLPArray $ getRLPObjects arrayData, nextRest)
  where
    dataLength::Word8
    dataLength = x - 192
    (arrayData, nextRest) = splitAtWithError (fromIntegral dataLength) rest
rlpSplit (x:len:rest) | x > 247 && x < 249 =
  (RLPArray $ getRLPObjects arrayData, nextRest)
  where
    dataLength = len
    (arrayData, nextRest) = splitAtWithError (fromIntegral dataLength) rest
rlpSplit (249:len1:len2:rest) =
  (RLPArray $ getRLPObjects arrayData, nextRest)
  where
    len = (fromIntegral len1) `shift` 8 + fromIntegral len2
    (arrayData, nextRest) = splitAtWithError len rest
rlpSplit (128:rest) =
  (RLPNumber 0, rest)
rlpSplit (129:val:rest) =
  (RLPNumber $ fromIntegral val, rest)
rlpSplit (130:val1:val2:rest) =
  (RLPNumber $ (shift (fromIntegral val1) 8 + fromIntegral val2), rest)
rlpSplit (x:rest) | x >= 131 && x <= 128+55 =
  (RLPString $ w2c <$> strList, nextRest)
  where
    strLength = x-128
    (strList, nextRest) = splitAtWithError (fromIntegral strLength) rest
rlpSplit (x:len:rest) | x >= 183 && x <= 184 =
  (RLPString $ w2c <$> strList, nextRest)
  where
    strLength = len
    (strList, nextRest) = splitAtWithError (fromIntegral strLength) rest
rlpSplit (x:rest) | x < 128 =
  (RLPNumber $ fromIntegral x, rest)
rlpSplit x = error ("Missing case in rlpSplit: " ++ show x)

getRLPObjects::[Word8]->[RLPObject]
getRLPObjects [] = []
getRLPObjects theData = obj:getRLPObjects rest
  where
    (obj, rest) = rlpSplit theData

int2Bytes::Int->[Word8]
int2Bytes val | val < 256 = [fromIntegral val]
int2Bytes val | val < 65536 = 
  map fromIntegral [0xFF .&. (val16 `shiftR` 8), 0xFF .&. val16]
  where val16 = fromIntegral val::Word16

rlp2Bytes::RLPObject->[Word8]
rlp2Bytes (RLPNumber 0) = [0x80]
rlp2Bytes (RLPNumber val) | val < 128 = [fromIntegral val]
rlp2Bytes (RLPNumber val) | val < 65536 =
  0x80 + fromIntegral (length bytes):bytes
  where
    bytes = int2Bytes val
rlp2Bytes (RLPString s) | length s <= 55 = (0x80 + fromIntegral (length s):(c2w <$> s))
rlp2Bytes (RLPString s) | length s < 65536 =
  [0xB7 + fromIntegral (length bytes)] ++ bytes ++ (c2w <$> s)
  where
    bytes = int2Bytes $ length s
rlp2Bytes (RLPArray innerObjects) =
  if length innerBytes <= 55
  then (0xC0 + fromIntegral (length innerBytes):innerBytes)
  else let lenBytes = int2Bytes $ length innerBytes
       in [0xF7 + fromIntegral (length lenBytes)] ++ lenBytes ++ innerBytes
  where
    innerBytes = concat $ rlp2Bytes <$> innerObjects
rlp2Bytes obj = error ("Missing case in rlp2Bytes: " ++ show obj)

getIntegerBytes::Integer->[Word8]
getIntegerBytes 0 = []
getIntegerBytes val = getIntegerBytes (val `quot` 256) ++ [fromIntegral $ val .&. 0xFF]

rlpNumber::Integer->RLPObject
rlpNumber x | x < 128 = RLPNumber $ fromIntegral x
rlpNumber x = RLPString $ w2c <$> getIntegerBytes x

getNumber::RLPObject->Integer
getNumber (RLPNumber n) = fromIntegral n
getNumber (RLPString s) = byteString2Integer $ B.pack $ map c2w s

rlpDeserialize::B.ByteString->RLPObject
rlpDeserialize s =
  case rlpSplit $ B.unpack s of
    (o, []) -> o
    _ -> error ("parse error converting ByteString to an RLP Object: " ++ show s)


instance RLPSerializable String where
  rlpEncode = error("rlpEncode undefined")
  rlpDecode (RLPString s) = s
  rlpDecode (RLPNumber n) = [w2c $ fromIntegral n]
  rlpDecode (RLPArray _) = error "Malformed RLP in call to rlpDecode for String: RLPObject is an array."
