{-# LANGUAGE OverloadedStrings #-}

module Blockchain.SigningTools (
  signTransaction,
  whoSignedThisTransaction
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Binary
import Data.ByteString.Internal
import Network.Haskoin.Internals hiding (Address)
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.ExtendedECDSA

import Blockchain.Data.Address
import Blockchain.Data.Transaction
import qualified Blockchain.Colors as CL
import Blockchain.Format
import Blockchain.Data.RLP
import Blockchain.SHA
import Blockchain.Data.SignedTransaction
import Blockchain.Util

--import Debug.Trace


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

whoSignedThisTransaction::SignedTransaction->Address
whoSignedThisTransaction SignedTransaction{unsignedTransaction=ut, v=v', r=r', s=s'} = 
    pubKey2Address (getPubKeyFromSignature xSignature theHash)
        where
          xSignature = ExtendedSignature (Signature (fromInteger r') (fromInteger s')) (0x1c == v')
          SHA theHash = hash (rlpSerialize $ rlpEncode ut)
