
module TransactionReceipt(
  TransactionReceipt(..),
  PostTransactionState(..)
  ) where

import Colors
import Format
import SignedTransaction
import RLP

data PostTransactionState = PostTransactionState String deriving (Show)

instance RLPSerializable PostTransactionState where
  rlpDecode (RLPString x) = PostTransactionState x
  rlpDecode x = error ("rlpDecode for PostTransactionState missing case: " ++ show x)
  rlpEncode (PostTransactionState x) = RLPString x

data TransactionReceipt =
  TransactionReceipt {
    theTransaction::SignedTransaction,
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
  rlpDecode x = error $ "Missing case in rlpDecode for TransactionReceipt: " ++ format x
  
  rlpEncode TransactionReceipt{
    theTransaction=t,
    postTransactionState=p,
    cumulativeGasUsed=gasUsed} =
    RLPArray [rlpEncode t, rlpEncode p, rlpEncode gasUsed]

