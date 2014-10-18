
module TransactionReceipt(
  TransactionReceipt(..),
  PostTransactionState(..)
  ) where

import Colors
import Format
import Transaction
import RLP

data PostTransactionState = PostTransactionState String deriving (Show)

instance RLPSerializable PostTransactionState where
  rlpDecode (RLPString x) = PostTransactionState x
  rlpDecode x = error ("rlpDecode for PostTransactionState missing case: " ++ show x)
  rlpEncode (PostTransactionState x) = RLPString x

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
      postTransactionState = rlpDecode pts,
      cumulativeGasUsed = rlpDecode gasUsed
      }
  rlpEncode TransactionReceipt{
    theTransaction=t,
    postTransactionState=p,
    cumulativeGasUsed=gasUsed} =
    RLPArray [rlpEncode t, rlpEncode p, rlpEncode gasUsed]

