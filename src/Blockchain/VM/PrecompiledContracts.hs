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
        maybePubKey = getPubKeyFromSignature (ExtendedSignature (Signature r s) (v == 28)) h
    in
     case (v >= 27, v <= 28, maybePubKey) of
       (True, True, Just pubKey) ->
         B.pack [0,0,0,0,0,0,0,0,0,0,0,0] `B.append` BL.toStrict (encode $ pubKey2Address pubKey)
       _ -> B.pack (replicate 32 0)

ripemd::B.ByteString->B.ByteString
ripemd input =
  B.replicate 12 0 `B.append` RIPEMD.hash input

sha2::B.ByteString->B.ByteString
sha2 input =
--    let val = fromInteger $ byteString2Integer $ B.take 32 input
--    in
     SHA2.hash SHA2.SHA256 input

callPrecompiledContract::Word160->B.ByteString->VMM B.ByteString
callPrecompiledContract 0 inputData = return B.empty

callPrecompiledContract 1 inputData = do
  useGas gECRECOVER
  return $ ecdsaRecover inputData

callPrecompiledContract 2 inputData = do
  useGas $ gSHA256BASE + gSHA256WORD*(ceiling $ fromIntegral (B.length inputData)/(32::Double))
  return $ sha2 inputData

callPrecompiledContract 3 inputData = do
  useGas $ gRIPEMD160BASE +
    gRIPEMD160WORD*(ceiling $ fromIntegral (B.length inputData)/(32::Double))
  return $ ripemd inputData

callPrecompiledContract 4 inputData = do
  useGas $ gIDENTITYBASE +
    gIDENTITYWORD*(ceiling $ fromIntegral (B.length inputData)/(32::Double))
  return inputData

callPrecompiledContract x _ = error $ "missing precompiled contract: " ++ show x
