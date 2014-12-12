{-# LANGUAGE OverloadedStrings #-}

module Data.GenesisBlock (
                      initializeGenesisBlock,
                      initializeStateDB
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import qualified Data.ByteString as B
import Data.Functor
import Data.Time.Clock.POSIX

import Database.MerklePatricia

import Data.Block
import Context
import Data.Address
import Data.AddressState
import DB.ModifyStateDB
import SHA

--import Debug.Trace

--startingRoot::B.ByteString
--(startingRoot, "") = B16.decode "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
                     --"bc36789e7a1e281436464229828f817d6612f7b477d66591ff96a9e064bcc98a"                                                                                                                   

initializeBlankStateDB::ContextM ()
initializeBlankStateDB = do
  cxt <- get
  liftIO $ runResourceT $
         initializeBlank (stateDB cxt)
  put cxt{stateDB=(stateDB cxt){stateRoot=emptyTriePtr}}

initializeStateDB::ContextM ()
initializeStateDB = do
  initializeBlankStateDB

  let addresses = Address <$> [
                        0x51ba59315b3a95761d0863b05ccc7a7f54703d99,
                        0xe6716f9544a56c530d868e4bfbacb172315bdead,
                        0xb9c015918bdaba24b4ff057a92a3873d6eb201be,
                        0x1a26338f0d905e295fccb71fa9ea849ffa12aaf4,
                        0x2ef47100e0787b915105fd5e3f4ff6752079d5cb,
                        0xcd2a3d9f938e13cd947ec05abc7fe734df8dd826,
                        0x6c386a4b26f73c802f34673f7248bb118f97424a,
                        0xe4157b34ea9615cfbde6b4fda419828124b70c78
                       ]

  putAddressStates addresses blankAddressState{balance=0x0100000000000000000000000000000000000000000000000000}

initializeGenesisBlock::ContextM Block
initializeGenesisBlock = do
  initializeStateDB
  cxt <- get
  let genesisBlock = Block {
               blockData = 
                   BlockData {
                       parentHash = SHA 0,
                       unclesHash = hash (B.pack [0xc0]), 
                       coinbase = Address 0,
                       bStateRoot = stateRoot $ stateDB cxt,
                       transactionsRoot = emptyTriePtr,
                       receiptsRoot = emptyTriePtr,
                       logBloom = B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],         
                       difficulty = 0x020000, --1 << 17
                       number = 0,
                       gasLimit = 1000000,
                       gasUsed = 0,
                       timestamp = posixSecondsToUTCTime 0,
                       extraData = 0,
                       nonce = hash $ B.pack [42]
               },
               receiptTransactions=[],
               blockUncles=[]
             }
  putBlock genesisBlock
  return genesisBlock




