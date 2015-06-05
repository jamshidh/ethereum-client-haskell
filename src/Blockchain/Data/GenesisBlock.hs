{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Blockchain.Data.GenesisBlock (
                      initializeGenesisBlock,
                      initializeStateDB
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Functor
import Data.Time.Clock.POSIX

import Blockchain.Database.MerklePatricia

import Blockchain.Constants
import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.DiffDB
import Blockchain.DB.ModifyStateDB
import Blockchain.DBM
import Blockchain.ExtWord
import Blockchain.SHA


import Blockchain.Format
import Blockchain.Data.RLP

--import Debug.Trace

--startingRoot::B.ByteString
--(startingRoot, "") = B16.decode "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
                     --"bc36789e7a1e281436464229828f817d6612f7b477d66591ff96a9e064bcc98a"                                                                                                                   

initializeBlankStateDB::ContextM ()
initializeBlankStateDB = do
  dbs <- lift get
  liftIO $ runResourceT $
         initializeBlank (stateDB dbs)
  lift $ put dbs{stateDB=(stateDB dbs){stateRoot=emptyTriePtr}}

initializeStateDB::ContextM ()
initializeStateDB = do
  initializeBlankStateDB

  let canonicalAddressInfo =
        [
          (0x0000000000000000000000000000000000000001, 1 * wei),
          (0x0000000000000000000000000000000000000002, 1 * wei),
          (0x0000000000000000000000000000000000000003, 1 * wei),
          (0x0000000000000000000000000000000000000004, 1 * wei),
          (0xdbdbdb2cbd23b783741e8d7fcf51e459b497e4a6, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xe6716f9544a56c530d868e4bfbacb172315bdead, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xb9c015918bdaba24b4ff057a92a3873d6eb201be, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0x1a26338f0d905e295fccb71fa9ea849ffa12aaf4, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0x2ef47100e0787b915105fd5e3f4ff6752079d5cb, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xcd2a3d9f938e13cd947ec05abc7fe734df8dd826, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0x6c386a4b26f73c802f34673f7248bb118f97424a, 1606938044258990275541962092341162602522202993782792835301376 * wei),
          (0xe4157b34ea9615cfbde6b4fda419828124b70c78, 1606938044258990275541962092341162602522202993782792835301376 * wei)
        ]

  let alternateAddresses =
        [
          0xaced1ce9bb193d4270acf8738942ac7d008f22b4, -- ace dice, 1b4ad 
          0xf1abb0d8af6f3de43cf05eb3d9458c95e79f30a0, -- flab bod, 3aa58
          0xba5e10071204f37931769d7afa454bc82e1eb4cd, -- base 100, 3f30b
          0xa1e5acc2a0c6efa671d9e27c4faf60f22fc50de0, -- ale sac, cb450
          0xbade1f4d04f56f13381b6e3dc0e78479fe2563c2, -- bad elf, cb60c
          0xe1fd1e0689bc35a4b7e531f50f96d77b02149dbc, -- elf die, e78bf
          0xdad1005e6487a99422b22a9db665e01148add52b, -- dad loose, 78c2db
          0xfa15efeef38db9ff6f7eb862e3402c66ee0e7951, -- false fee, 1e6392
          0xfadedcabf08aead902061c146c5458e5bea1ce9f, --faded cab, 2554ba
          0x5eedfab1e5e0ed81674dc6b503c6c89d413ccbc9, -- seed fable, 675888
          0x1dea5c01dd17dee05b08c89e0753722f52f6d2f1, -- idea scold, 7e1ada

          0xe1fd0d4a52b75a694de8b55528ad48e2e2cf7859, -- 1dd885a423f4e212740f116afa66d40aafdbb3a381079150371801871d9ea281
          0x1001e1077880d7e6ca43919b79eb595881a3f1f7, -- 1dd885a423f4e212740f116afa66d40aafdbb3a381079150371801871d9eac22
          0xbed1006aba1aff1b832f42b05239bc53c0f3aaba, -- 1dd885a423f4e212740f116afa66d40aafdbb3a381079150371801871d9eac31
          0xada0ca69254ddc48fa52cb7b26e65ecce2f7c63c -- 1dd885a423f4e212740f116afa66d40aafdbb3a381079150371801871d9eb1bb
        ]

  let alternateAddressInfo = map (, 1606938044258990275541962092341162602522202993782792835301376*wei) alternateAddresses

  cxt <- get
  
  let addressInfo = if useAlternateGenesisBlock cxt then alternateAddressInfo else canonicalAddressInfo
  
  forM_ addressInfo $ \(address, balance) ->
    lift $ putAddressState (Address address) blankAddressState{addressStateBalance=balance}

initializeGenesisBlock::ContextM Block
initializeGenesisBlock = do
  initializeStateDB
  dbs <- lift get
  let genesisBlock = Block {
               blockBlockData = 
                   BlockData {
                       blockDataParentHash = SHA 0,
                       blockDataUnclesHash = hash (B.pack [0xc0]), 
                       blockDataCoinbase = Address 0,
                       --blockDataStateRoot = SHAPtr $ fst $ B16.decode "9178d0f23c965d81f0834a4c72c6253ce6830f4022b1359aaebfc1ecba442d4e", -- stateRoot $ stateDB dbs,
                       blockDataStateRoot = stateRoot $ stateDB dbs,
                       blockDataTransactionsRoot = emptyTriePtr,
                       blockDataReceiptsRoot = emptyTriePtr,
                       blockDataLogBloom = B.replicate 256 0,
                       blockDataDifficulty = 0x020000, --1 << 17
                       blockDataNumber = 0,
                       blockDataGasLimit = 3141592,
                       blockDataGasUsed = 0,
                       blockDataTimestamp = posixSecondsToUTCTime 0,
                       blockDataExtraData = 0,
                       blockDataMixHash = SHA 0,
                       blockDataNonce = 42 -- hash $ B.pack [42]
               },
               blockReceiptTransactions=[],
               blockBlockUncles=[]
             }
  genBlkId <- lift $ putBlock genesisBlock
  ctx <- lift get
  lift $ sqlDiff genBlkId 0 emptyTriePtr (stateRoot $ stateDB ctx)

  return genesisBlock




