{-# LANGUAGE OverloadedStrings #-}

module Block (
  BlockData(..),
  Block(..),
  getBlockFromRLP
  ) where

import Crypto.Hash.SHA3
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Base16
import Data.ByteString.Internal
import Data.Functor
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import Numeric

import ExtendedECDSA

import Address
import Colors
import Format
import PrettyBytes
import RLP
import SHA
import Transaction
import TransactionReceipt
import Util

import Debug.Trace

data BlockData = BlockData {
  parentHash::SHA,
  unclesHash::SHA,
  coinbase::Address,
  stateRoot::SHA,
  transactionsTrie::SHA,
  difficulty::Integer,
  number::Integer,
  minGasPrice::Integer,
  gasLimit::Integer,
  gasUsed::Integer,
  timestamp::UTCTime,
  extraData::Integer,
  nonce::SHA
} deriving (Show)

data Block = Block {
  blockData::BlockData,
  receiptTransactions::[TransactionReceipt],
  blockUncles::[BlockData]
  } deriving (Show)

instance Format Block where
  format b@Block{blockData=bd, receiptTransactions=[], blockUncles=[]} =
    blue ("Block #" ++ show (number bd) ++ " (no transactions, no uncles): ") ++
    BC.unpack (encode (hash 256 (B.pack $ rlp2Bytes $ rlpEncode b))) ++
    format bd

instance RLPSerializable Block where
  rlpDecode (RLPArray [blockData, RLPArray transactionReceipts, RLPArray uncles]) =
    Block (rlp2BlockData blockData) (getTransactionReceiptFromRLP <$> transactionReceipts) []
  rlpDecode (RLPArray arr) = error ("getBlockFromRLP called on object with wrong amount of data, length arr = " ++ show arr)
  rlpDecode x = error ("getBlockFromRLP called on non block object: " ++ show x)
  rlpEncode Block{blockData=bd, receiptTransactions=[], blockUncles=[]} =
    RLPArray [rlpEncode bd, RLPArray [], RLPArray []]

--TODO remove this
getBlockFromRLP::RLPObject->Block
getBlockFromRLP = rlpDecode

instance RLPSerializable BlockData where
  rlpDecode = undefined
  rlpEncode bd =
    RLPArray [
      rlpEncode $ parentHash bd,
      rlpEncode $ unclesHash bd,
      address2RLP $ coinbase bd,
      rlpEncode $ stateRoot bd,
      rlpEncode $ transactionsTrie bd,
      rlpNumber $ difficulty bd,
      rlpNumber $ number bd,
      rlpNumber $ minGasPrice bd,
      rlpNumber $ gasLimit bd,
      rlpNumber $ gasUsed bd,
      rlpNumber $ round $ utcTimeToPOSIXSeconds $ timestamp bd,
      rlpNumber $ extraData bd,
      rlpEncode $ nonce bd
      ]


instance Format BlockData where
  format b = 
    "        parentHash: " ++ format (parentHash b) ++ "\n" ++
    "        unclesHash: " ++ format (unclesHash b) ++ "\n" ++
    "        coinbase: " ++ format (coinbase b) ++ "\n" ++
    "        stateRoot: " ++ format (stateRoot b) ++ "\n" ++
    "        transactionsTrie: " ++ format (transactionsTrie b) ++ "\n" ++
    "        difficulty: " ++ show (difficulty b) ++ "\n" ++
    "        minGasPrice: " ++ show (minGasPrice b) ++ "\n" ++
    "        gasLimit: " ++ show (gasLimit b) ++ "\n" ++
    "        gasUsed: " ++ show (gasUsed b) ++ "\n" ++
    "        timestamp: " ++ show (timestamp b) ++ "\n" ++
    "        extraData: " ++ show (extraData b) ++ "\n" ++
    "        nonce: " ++ format (nonce b) ++ "\n"

rlp2BlockData::RLPObject->BlockData
rlp2BlockData (RLPArray [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13]) =
  BlockData {
    parentHash = rlpDecode v1,
    unclesHash = rlpDecode v2,
    coinbase = rlp2Address v3,
    stateRoot = rlpDecode v4,
    transactionsTrie = rlpDecode v5,
    difficulty = fromIntegral $ getNumber v6,
    number = fromIntegral $ getNumber v7,
    minGasPrice = fromIntegral $ getNumber v8,
    gasLimit = fromIntegral $ getNumber v9,
    gasUsed = fromIntegral $ getNumber v10,
    timestamp = posixSecondsToUTCTime $ fromIntegral $ getNumber v11,
    extraData = fromIntegral $ getNumber v12,
    nonce = rlpDecode v13
    }  
rlp2BlockData (RLPArray arr) = error ("rlp2BlockData called on object with wrong amount of data, length arr = " ++ show arr)
rlp2BlockData x = error ("rlp2BlockData called on non block object: " ++ show x)

getTransactionReceiptFromRLP::RLPObject->TransactionReceipt
getTransactionReceiptFromRLP (RLPArray [t, pts, gasUsed]) =
  TransactionReceipt {
    theTransaction = rlp2Transaction t,
    postTransactionState = PostTransactionState,
    cumulativeGasUsed = getNumber gasUsed
    }

