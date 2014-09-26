
module TransactionReceipt(
  TransactionReceipt(..),
  PostTransactionState(..)
  ) where

import Colors
import Format
import Transaction
import RLP

data PostTransactionState = PostTransactionState deriving (Show)

instance RLPSerializable PostTransactionState where
  rlpDecode x = PostTransactionState
  rlpEncode x = RLPArray []

data TransactionReceipt =
  TransactionReceipt {
    theTransaction::Transaction,
    postTransactionState::PostTransactionState,
    cumulativeGasUsed::Integer
    } deriving (Show)

instance Format TransactionReceipt where
  format (TransactionReceipt t p gasUsed) =
    blue "TransactionReceipt: " ++ show gasUsed ++ "\n" ++ format t ++ "\n" ++ show p

instance RLPSerializable TransactionReceipt where
  rlpDecode (RLPArray [t, pts, gasUsed]) =
    TransactionReceipt {
      theTransaction = rlpDecode t,
      postTransactionState = PostTransactionState,
      cumulativeGasUsed = getNumber gasUsed
      }
  rlpEncode TransactionReceipt{
    theTransaction=t,
    postTransactionState=p,
    cumulativeGasUsed=gasUsed} =
    RLPArray [rlpEncode t, rlpEncode p, rlpNumber gasUsed]

