{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Data.GenesisBlock (
                      initializeGenesisBlock,
                      initializeStateDB
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import qualified Data.ByteString as B
import Data.Functor
import Data.Time.Clock.POSIX

import Blockchain.Database.MerklePatricia

import Blockchain.Constants
import Blockchain.Data.Block
import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressState
import Blockchain.DB.ModifyStateDB
import Blockchain.SHA

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


  let addressInfo =
        [
          (0xdbdbdb2cbd23b783741e8d7fcf51e459b497e4a6, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xe6716f9544a56c530d868e4bfbacb172315bdead, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xb9c015918bdaba24b4ff057a92a3873d6eb201be, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0x1a26338f0d905e295fccb71fa9ea849ffa12aaf4, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0x2ef47100e0787b915105fd5e3f4ff6752079d5cb, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xcd2a3d9f938e13cd947ec05abc7fe734df8dd826, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0x6c386a4b26f73c802f34673f7248bb118f97424a, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xe4157b34ea9615cfbde6b4fda419828124b70c78, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xb0afc46d9ce366d06ab4952ca27db1d9557ae9fd, 154162184 * finney),
          (0xf6b1e9dc460d4d62cc22ec5f987d726929c0f9f0, 102774789 * finney),
          (0xcc45122d8b7fa0b1eaa6b29e0fb561422a9239d0, 51387394 * finney),
          (0xb7576e9d314df41ec5506494293afb1bd5d3f65d, 69423399 * finney)
        ]

  forM_ addressInfo $ \(address, balance) -> 
    putAddressState (Address address) blankAddressState{balance=balance}

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




