{-# LANGUAGE OverloadedStrings #-}

module Transaction (
  Transaction(..),
  signTransaction
  ) where

import Crypto.Hash.SHA3
import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString.Base16
import Data.ByteString.Internal
import Data.Word
import Network.Haskoin.Internals hiding (Address)
import Numeric

import ExtendedECDSA

import Address
import Format
import PrettyBytes
import RLP
import Util

import Debug.Trace


data Transaction =
  Transaction {
    tNonce::Integer,
    gasPrice::Integer,
    tGasLimit::Int,
    to::Address,
    value::Integer,
    tInit::Integer,
    v::Word8,
    r::Integer,
    s::Integer
    } deriving (Show)

addLeadingZerosTo64::String->String
addLeadingZerosTo64 x = replicate (64 - length x) '0' ++ x

signTransaction::Monad m=>PrvKey->Transaction->SecretT m Transaction
signTransaction privKey t = do
  ExtendedSignature signature yIsOdd <- extSignMsg theHash privKey

  return $ t {
    v = if yIsOdd then 0x1c else 0x1b,
    r =
      case decode $ B.pack $ map c2w $ addLeadingZerosTo64 $ showHex (sigR signature) "" of
        (val, "") -> byteString2Integer val
        _ -> error ("error: sigR is: " ++ showHex (sigR signature) ""),
    s = 
      case decode $ B.pack $ map c2w $ addLeadingZerosTo64 $ showHex (sigS signature) "" of
        (val, "") -> byteString2Integer val
        _ -> error ("error: sigS is: " ++ showHex (sigS signature) "")
    }
  where
    theData = rlp2Bytes $
              RLPArray [
                rlpNumber $ tNonce t,
                rlpNumber $ gasPrice t,
                RLPNumber $ tGasLimit t,
                rlpAddress $ to t,
                rlpNumber $ value t,
                rlpNumber $ tInit t
                ]
    theHash = fromInteger $ byteString2Integer $ hash 256 $ B.pack theData
