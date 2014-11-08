
module Data.TransactionReceipt(
  TransactionReceipt(..),
  PostTransactionState(..)
  ) where

import Colors
import Format
import SHA
import Data.SignedTransaction
import Data.RLP

data PostTransactionState = PostTransactionState SHA deriving (Show)

instance RLPSerializable PostTransactionState where
  rlpDecode x = PostTransactionState $ rlpDecode x
  rlpEncode (PostTransactionState x) = rlpEncode x

data TransactionReceipt =
  TransactionReceipt {
    theTransaction::SignedTransaction,
    postTransactionState::PostTransactionState,
    cumulativeGasUsed::Integer
    } deriving (Show)

instance Format PostTransactionState where
  format (PostTransactionState x) = format x

instance Format TransactionReceipt where
  format (TransactionReceipt t p gasUsed) =
    blue "TransactionReceipt: " ++ show gasUsed ++ "\n" ++ format t ++ "\nPostTransactionState: " ++ format p

instance RLPSerializable TransactionReceipt where
  rlpDecode (RLPArray [t, pts, gasUsed]) =
    TransactionReceipt {
      theTransaction = rlpDecode t,
      postTransactionState = rlpDecode pts,
      cumulativeGasUsed = rlpDecode gasUsed
      }
  rlpDecode x = error $ "Missing case in rlpDecode for TransactionReceipt: " ++ format x
  
  rlpEncode TransactionReceipt{
    theTransaction=t,
    postTransactionState=p,
    cumulativeGasUsed=gasUsed} =
    RLPArray [rlpEncode t, rlpEncode p, rlpEncode gasUsed]

