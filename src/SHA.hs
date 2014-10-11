
module SHA (
  SHA(..),
  hash,
  rlp2Word512,
  word5122RLP,
  padZeros
  ) where

import qualified Crypto.Hash.SHA3 as C
import Data.Binary
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Network.Haskoin.Internals
import Numeric

import Colors
import Format
import RLP
import Util

newtype SHA = SHA Word256 deriving (Show, Eq)

instance Format SHA where
  format (SHA x) = yellow $ padZeros 64 $ showHex x ""

instance Binary SHA where
  put (SHA x) = sequence_ $ fmap put $ (integer2Bytes $ fromIntegral x)
  get = do
    bytes <- sequence $ replicate 32 get
    let byteString = B.pack bytes
    return (SHA $ fromInteger $ byteString2Integer byteString)

instance RLPSerializable SHA where
  rlpDecode (RLPString s) | length s == 32 = SHA $ decode $ BLC.pack s
  rlpDecode (RLPNumber 0) = SHA 0 --special case seems to be allowed, even if length of zeros is wrong
  rlpDecode x = error ("Missing case in rlpDecode for SHA: " ++ show x)
  --rlpEncode (SHA 0) = RLPNumber 0
  rlpEncode (SHA val) = RLPString $ BC.unpack $ fst $ B16.decode $ BC.pack $ padZeros 64 $ showHex val ""

hash::BC.ByteString->SHA
hash = SHA . fromIntegral . byteString2Integer . C.hash 256

--------------------- Word512 stuff

rlp2Word512::RLPObject->Word512
rlp2Word512 (RLPString s) | length s == 64 = decode $ BLC.pack s
rlp2Word512 x = error ("Missing case in rlp2Word512: " ++ show x)

word5122RLP::Word512->RLPObject
word5122RLP val = RLPString $ BLC.unpack $ encode val

