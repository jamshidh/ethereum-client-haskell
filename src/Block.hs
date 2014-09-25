{-# LANGUAGE OverloadedStrings #-}

module Block (
  BlockData(..),
  Block(..),
--  rlp2BlockData,
  getBlockFromRLP
  ) where

import Crypto.Hash.SHA3
import Data.Bits
import qualified Data.ByteString as B
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
import Transaction
import TransactionReceipt
import Util

import Debug.Trace

data BlockData = BlockData {
  parentHash::Integer,
  unclesHash::Integer,
  coinbase::Address,
  stateRoot::Integer,
  transactionsTrie::Integer,
  difficulty::Integer,
  number::Integer,
  minGasPrice::Integer,
  gasLimit::Integer,
  gasUsed::Integer,
  timestamp::UTCTime,
  extraData::Integer,
  nonce::Integer
} deriving (Show)

data Block = Block {
  blockData::BlockData,
  receiptTransactions::[TransactionReceipt],
  blockUncles::[BlockData]
  } deriving (Show)

instance Format Block where
  format Block{blockData=b, receiptTransactions=[], blockUncles=[]} =
    blue "Block(no transactions, no uncles):\n" ++ format b

instance Format BlockData where
  format b = 
    "        parentHash: " ++ show (parentHash b) ++ "\n" ++
    "        unclesHash: " ++ show (unclesHash b) ++ "\n" ++
    "        coinbase: " ++ format (coinbase b) ++ "\n" ++
    "        stateRoot: " ++ show (stateRoot b) ++ "\n" ++
    "        transactionsTrie: " ++ show (transactionsTrie b) ++ "\n" ++
    "        difficulty: " ++ show (difficulty b) ++ "\n" ++
    "        number: " ++ show (number b) ++ "\n" ++
    "        minGasPrice: " ++ show (minGasPrice b) ++ "\n" ++
    "        gasLimit: " ++ show (gasLimit b) ++ "\n" ++
    "        gasUsed: " ++ show (gasUsed b) ++ "\n" ++
    "        timestamp: " ++ show (timestamp b) ++ "\n" ++
    "        extraData: " ++ show (extraData b) ++ "\n" ++
    "        nonce: " ++ show (nonce b) ++ "\n"

rlp2BlockData::RLPObject->BlockData
rlp2BlockData (RLPArray [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13]) =
  BlockData {
    parentHash = fromIntegral $ getNumber v1,
    unclesHash = fromIntegral $ getNumber v2,
    coinbase = rlp2Address v3,
    stateRoot = fromIntegral $ getNumber v4,
    transactionsTrie = fromIntegral $ getNumber v5,
    difficulty = fromIntegral $ getNumber v6,
    number = fromIntegral $ getNumber v7,
    minGasPrice = fromIntegral $ getNumber v8,
    gasLimit = fromIntegral $ getNumber v9,
    gasUsed = fromIntegral $ getNumber v10,
    timestamp = posixSecondsToUTCTime $ fromIntegral $ getNumber v11,
    extraData = fromIntegral $ getNumber v12,
    nonce =fromIntegral $ getNumber v13
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

getBlockFromRLP::RLPObject->Block
getBlockFromRLP (RLPArray [blockData, RLPArray transactionReceipts, RLPArray uncles]) =
  Block (rlp2BlockData blockData) (getTransactionReceiptFromRLP <$> transactionReceipts) []
getBlockFromRLP (RLPArray arr) = error ("getBlockFromRLP called on object with wrong amount of data, length arr = " ++ show arr)
getBlockFromRLP x = error ("getBlockFromRLP called on non block object: " ++ show x)

