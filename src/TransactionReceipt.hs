
module TransactionReceipt(
  TransactionReceipt(..),
  PostTransactionState(..)
  ) where

import Transaction

data PostTransactionState = PostTransactionState deriving (Show)

data TransactionReceipt =
  TransactionReceipt {
    theTransaction::Transaction,
    postTransactionState::PostTransactionState,
    cumulativeGasUsed::Integer
    } deriving (Show)

