{-# LANGUAGE OverloadedStrings #-}

module Transaction (
  Transaction(..)
  ) where

import Address
import Code
import Colors
import Format
import RLP
import Util
--import VM

--import Debug.Trace


data Transaction =
  Transaction {
    tNonce::Integer,
    gasPrice::Integer,
    tGasLimit::Integer,
    to::Address,
    value::Integer,
    tInit::Code
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
      "tInit: " ++ tab ("\n" ++ format (tInit x)) ++ "\n")

instance RLPSerializable Transaction where
  rlpDecode (RLPArray [n, gp, gl, toAddr, val, i]) =
    Transaction {
      tNonce = rlpDecode n,
      gasPrice = rlpDecode gp,
      tGasLimit = fromInteger $ rlpDecode gl,
      to = rlpDecode toAddr,
      value = rlpDecode val,
      tInit = rlpDecode i
      }
  rlpDecode x = error ("rlpDecode for Transaction called on non block object: " ++ show x)

  rlpEncode t =
      RLPArray [
        rlpEncode $ tNonce t,
        rlpEncode $ gasPrice t,
        rlpEncode $ toInteger $ tGasLimit t,
        rlpEncode $ to t,
        rlpEncode $ value t,
        rlpEncode $ tInit t
        ]
