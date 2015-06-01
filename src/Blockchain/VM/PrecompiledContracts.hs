{-# LANGUAGE OverloadedStrings #-}

module Blockchain.VM.PrecompiledContracts (
  callPrecompiledContract
  ) where

import Prelude hiding (LT, GT, EQ)

import qualified Codec.Digest.SHA as SHA2
import qualified Crypto.Hash.RIPEMD160 as RIPEMD
import Data.Binary hiding (get, put)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Haskoin.Internals (Signature(..))
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.Address
import Blockchain.ExtendedECDSA
import Blockchain.ExtWord
import Blockchain.Util
import Blockchain.VM.OpcodePrices
import Blockchain.VM.VMM


--import Debug.Trace

ecdsaRecover::B.ByteString->B.ByteString
ecdsaRecover input =
    let h = fromInteger $ byteString2Integer $ B.take 32 input
        v = byteString2Integer $ B.take 32 $ B.drop 32 input
        r = fromInteger $ byteString2Integer $ B.take 32 $ B.drop 64 input
        s = fromInteger $ byteString2Integer $ B.take 32 $ B.drop 96 input
    in
     if (r == 0) || (v < 27) || (v > 28)
     then B.pack (replicate 32 0)
     else 
       let pubKey = getPubKeyFromSignature (ExtendedSignature (Signature r s) (v == 28)) h
       in B.pack [0,0,0,0,0,0,0,0,0,0,0,0] `B.append` BL.toStrict (encode $ pubKey2Address pubKey)     

ripemd::B.ByteString->B.ByteString
ripemd input =
  B.replicate 12 0 `B.append` RIPEMD.hash input

sha2::B.ByteString->B.ByteString
sha2 input =
--    let val = fromInteger $ byteString2Integer $ B.take 32 input
--    in
     SHA2.hash SHA2.SHA256 input

callPrecompiledContract::Word160->B.ByteString->VMM B.ByteString
callPrecompiledContract 0 _ = return B.empty
callPrecompiledContract n inputData
  = do
    useGas gasPrice
    return $! contractFunc inputData
    where
      (gasPrice, contractFunc) =
        let g = opGasItemPrice
            inputWords = countBytesToWords (B.length inputData)
        in case n of
          1 -> (g ECRECOVER, ecdsaRecover)
          2 -> (g SHA256BASE    + g SHA256WORD    * inputWords, sha2)
          3 -> (g RIPEMD160BASE + g RIPEMD160WORD * inputWords, ripemd)
          4 -> (g IDENTITYBASE  + g IDENTITYWORD  * inputWords, id)
          m -> error $ "missing precompiled contract: " ++ show m
