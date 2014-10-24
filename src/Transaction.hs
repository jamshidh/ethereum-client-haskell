{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Transaction (
  Transaction(..),
  signTransaction,
  whoSignedThisTransaction
  ) where

import qualified Crypto.Hash.SHA3 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Binary
import Data.ByteString.Internal
import Data.Functor
import Data.Word
import Network.Haskoin.Internals hiding (Address)
import Numeric

import ExtendedECDSA

import Address
import Colors
import Format
import RLP
import Util
import VM

--import Debug.Trace


data Transaction =
  Transaction {
    tNonce::Integer,
    gasPrice::Integer,
    tGasLimit::Int,
    to::Address,
    value::Integer,
    tInit::B.ByteString,
    v::Word8,
    r::Integer,
    s::Integer
    } deriving (Show)

instance Format Transaction where
  format x =
    blue "Transaction" ++
    tab (
      "\n" ++
      "tNonce: " ++ show (tNonce x) ++ "\n" ++
      "gasPrice: " ++ show (gasPrice x) ++ "\n" ++
      "tGasLimit: " ++ show (tGasLimit x) ++ "\n" ++
      "to: " ++ format (to x) ++ "\n" ++
      "value: " ++ show (value x) ++ "\n" ++
      "tInit: " ++ tab ("\n" ++ showCode (tInit x)) ++ "\n" ++
      "v: " ++ show (v x) ++ "\n" ++
      "r: " ++ show (r x) ++ "\n" ++
      "s: " ++ show (s x) ++ "\n")

addLeadingZerosTo64::String->String
addLeadingZerosTo64 x = replicate (64 - length x) '0' ++ x

signTransaction::Monad m=>PrvKey->Transaction->SecretT m Transaction
signTransaction privKey t = do
  ExtendedSignature signature yIsOdd <- extSignMsg theHash privKey

  return $ t {
    v = if yIsOdd then 0x1c else 0x1b,
    r =
      case B16.decode $ B.pack $ map c2w $ addLeadingZerosTo64 $ showHex (sigR signature) "" of
        (val, "") -> byteString2Integer val
        _ -> error ("error: sigR is: " ++ showHex (sigR signature) ""),
    s = 
      case B16.decode $ B.pack $ map c2w $ addLeadingZerosTo64 $ showHex (sigS signature) "" of
        (val, "") -> byteString2Integer val
        _ -> error ("error: sigS is: " ++ showHex (sigS signature) "")
    }
  where
    theData = rlpSerialize $
              RLPArray [
                rlpEncode $ tNonce t,
                rlpEncode $ gasPrice t,
                rlpEncode $ toInteger $ tGasLimit t,
                address2RLP $ to t,
                rlpEncode $ value t,
                rlpEncode $ BC.unpack $ tInit t
                ]
    theHash = fromInteger $ byteString2Integer $ C.hash 256 theData


instance RLPSerializable Transaction where
  rlpDecode (RLPArray [n, gp, gl, toAddr, val, i, vVal, rVal, sVal]) =
    Transaction {
      tNonce = rlpDecode n,
      gasPrice = rlpDecode gp,
      tGasLimit = fromInteger $ rlpDecode gl,
      to = rlp2Address toAddr,
      value = rlpDecode val,
      tInit = BC.pack $ rlpDecode i,
      v = fromInteger $ rlpDecode vVal,
      r = rlpDecode rVal,
      s = rlpDecode sVal
      }
  rlpDecode x = error ("rlpDecode for Transaction called on non block object: " ++ show x)

  rlpEncode t =
      RLPArray [
        rlpEncode $ tNonce t,
        rlpEncode $ gasPrice t,
        rlpEncode $ toInteger $ tGasLimit t,
        address2RLP $ to t,
        rlpEncode $ value t,
        rlpEncode $ BC.unpack $ tInit t,
        rlpEncode $ toInteger $ v t,
        rlpEncode $ r t,
        rlpEncode $ s t
        ]

whoSignedThisTransaction::Transaction->Address
whoSignedThisTransaction t =
  pubKey2Address (getPubKeyFromSignature xSignature (fromInteger $ byteString2Integer $ C.hash 256 (theData t)))
      where
        xSignature = ExtendedSignature (Signature (fromInteger $ r t) (fromInteger $ s t)) (0x1c == v t)
        theData t = rlpSerialize $
              RLPArray [
                rlpEncode $ tNonce t,
                rlpEncode $ gasPrice t,
                rlpEncode $ toInteger $ tGasLimit t,
                address2RLP $ to t,
                rlpEncode $ value t,
                rlpEncode $ BC.unpack $ tInit t
                ]
  
