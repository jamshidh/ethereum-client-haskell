
module Transaction (
  Transaction(..),
  signTransaction
  ) where

import Crypto.Hash.SHA3
import Data.Bits
import qualified Data.ByteString as B
import Data.Word
import Network.Haskoin.Crypto

import Format
import PrettyBytes
import RLP

import Debug.Trace

data Word160 = Word160 Word32 Word32 Word32 Word32 Word32 deriving (Show)



data Transaction =
  Transaction {
    tNonce::Int,
    gasPrice::Integer,
    tGasLimit::Int,
    to::Integer,
    value::Integer,
    tInit::Int,
    v::Int,
    r::String,
    s::String
    } deriving (Show)

--I hate this, it is an ugly way to create an Integer from its component bytes.
--There should be an easier way....
--See http://stackoverflow.com/questions/25854311/efficient-packing-bytes-into-integers
stupidConvert::B.ByteString->Integer
stupidConvert x = stupidConvert' $ B.unpack x
  where
    stupidConvert'::[Word8]->Integer
    stupidConvert' [] = 0
    stupidConvert' (x:rest) = fromIntegral x `shift` (8 * length rest) + stupidConvert' rest


signTransaction::PrvKey->Transaction->Transaction
signTransaction privKey t =
  trace ("data: " ++ format (B.pack theData) ++ "\n") $
  trace ("hash: " ++ show theHash ++ "\n") $
  t {
    v = 0x1c,
    r = show $ sigR signature,
    s = show $ sigS signature
    }
  where
    theData = rlp2Bytes $
              RLPArray [
                RLPNumber $ tNonce t,
                rlpNumber $ gasPrice t,
                RLPNumber $ tGasLimit t,
                rlpNumber $ to t,
                rlpNumber $ value t,
                RLPNumber $ tInit t
                ]
    theHash = fromInteger $ stupidConvert $ hash 256 $ B.pack theData
    signature = detSignMsg theHash privKey
