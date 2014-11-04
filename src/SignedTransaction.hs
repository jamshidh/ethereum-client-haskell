{-# LANGUAGE OverloadedStrings #-}

module SignedTransaction (
  SignedTransaction(..),
  signTransaction,
  whoSignedThisTransaction
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Binary
import Data.ByteString.Internal
import Network.Haskoin.Internals hiding (Address)
import Numeric

import ExtendedECDSA

import Address
import Colors
import Format
import RLP
import SHA
import Transaction
import Util

--import Debug.Trace


data SignedTransaction =
  SignedTransaction {
      unsignedTransaction::Transaction,
      v::Word8,
      r::Integer,
      s::Integer
    } deriving (Show)

instance Format SignedTransaction where
  format SignedTransaction{unsignedTransaction = x, v=v', r=r', s=s'} =
      blue "Transaction" ++
           tab (
                "\n" ++
                "tNonce: " ++ show (tNonce x) ++ "\n" ++
                "gasPrice: " ++ show (gasPrice x) ++ "\n" ++
                "tGasLimit: " ++ show (tGasLimit x) ++ "\n" ++
                "to: " ++ format (to x) ++ "\n" ++
                "value: " ++ show (value x) ++ "\n" ++
                "tInit: " ++ tab ("\n" ++ format (tInit x)) ++ "\n" ++
                "v: " ++ show v' ++ "\n" ++
                "r: " ++ show r' ++ "\n" ++
                "s: " ++ show s' ++ "\n")

addLeadingZerosTo64::String->String
addLeadingZerosTo64 x = replicate (64 - length x) '0' ++ x

signTransaction::Monad m=>PrvKey->Transaction->SecretT m SignedTransaction
signTransaction privKey t = do
  ExtendedSignature signature yIsOdd <- extSignMsg theHash privKey

  return SignedTransaction{
               unsignedTransaction = t,
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
        SHA theHash = hash $ rlpSerialize $ rlpEncode t


instance RLPSerializable SignedTransaction where
  rlpDecode (RLPArray [n, gp, gl, toAddr, val, i, vVal, rVal, sVal]) =
    SignedTransaction {
      unsignedTransaction=rlpDecode $ RLPArray [n, gp, gl, toAddr, val, i],
      v = fromInteger $ rlpDecode vVal,
      r = rlpDecode rVal,
      s = rlpDecode sVal
      }
  rlpDecode x = error ("rlpDecode for Transaction called on non block object: " ++ show x)

  rlpEncode t =
      RLPArray [
        n, gp, gl, toAddr, val, i,
        rlpEncode $ toInteger $ v t,
        rlpEncode $ r t,
        rlpEncode $ s t
        ]
      where
        (RLPArray [n, gp, gl, toAddr, val, i]) = rlpEncode (unsignedTransaction t)

whoSignedThisTransaction::SignedTransaction->Address
whoSignedThisTransaction SignedTransaction{unsignedTransaction=ut, v=v', r=r', s=s'} =
    pubKey2Address (getPubKeyFromSignature xSignature theHash)
        where
          xSignature = ExtendedSignature (Signature (fromInteger r') (fromInteger s')) (0x1c == v')
          SHA theHash = hash (rlpSerialize $ rlpEncode ut)
