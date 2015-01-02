
module Blockchain.Data.TransactionReceipt(
  TransactionReceipt(..),
  PostTransactionState(..)
  ) where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Blockchain.Colors as CL
import Blockchain.Format
import Blockchain.SHA
import Blockchain.Data.SignedTransaction
import Blockchain.Data.RLP

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


{-
nonce: 0x,
gasPrice: 0x09184e72a000, --10000000000000
maxGas: 0x07d0
to: 0x9f840fe058ce3d84e319b8c747accc1e52f69426
, 0x
data: 0x00000000000000000000000000000000000000000000000000000000000003e800000000000000000000000000000000000000000000000000000000000007d000000000000000000000000000000000000000000000000000000000000004d2000\
0000000000000000000000000000000000000000000000000000000000001
, 0x1b
, 0x7012b0d1998a049ca767541add20b2a185a6eb0452f637ceb566de87056d9f03
, 0x17f015abaed48196c8d0290bab1aacf82a0e7c02c31618389b2a9990c2731793
-}






instance Format PostTransactionState where
  format (PostTransactionState x) = show $ pretty x

instance Format TransactionReceipt where
  format (TransactionReceipt t p gasUsed) =
    CL.blue "TransactionReceipt: " ++ show gasUsed ++ "\n" ++ format t ++ "\nPostTransactionState: " ++ format p

instance RLPSerializable TransactionReceipt where
  rlpDecode (RLPArray [t, pts, gasUsed]) =
    TransactionReceipt {
      theTransaction = rlpDecode t,
      postTransactionState = rlpDecode pts,
      cumulativeGasUsed = rlpDecode gasUsed
      }
  rlpDecode x = error $ "Missing case in rlpDecode for TransactionReceipt: " ++ show (pretty x)
  
  rlpEncode TransactionReceipt{
    theTransaction=t,
    postTransactionState=p,
    cumulativeGasUsed=gasUsed} =
    RLPArray [rlpEncode t, rlpEncode p, rlpEncode gasUsed]

