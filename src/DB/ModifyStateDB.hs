{-# LANGUAGE OverloadedStrings #-}

module DB.ModifyStateDB (
                      initializeBlankStateDB,
                      initializeStateDB,
                      addToBalance,
                      addNonce
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Functor
import Data.Maybe

import Context
import Data.Address
import Data.AddressState
import ExtDBs
import SHA

--import Debug.Trace

startingRoot::B.ByteString
(startingRoot, "") = B16.decode "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
                     --"bc36789e7a1e281436464229828f817d6612f7b477d66591ff96a9e064bcc98a"                                                                                                                   

startingAddressState::AddressState
startingAddressState =
      AddressState {
      addressStateNonce=0,
      balance= 0,
      contractRoot=Nothing,
      codeHash=hash B.empty
      }

initializeBlankStateDB::ContextM ()
initializeBlankStateDB = do
  stateDBPut startingRoot B.empty

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

  putAddressStates addresses startingAddressState{balance=0x0100000000000000000000000000000000000000000000000000}

  

putAddressStates::[Address]->AddressState->ContextM ()
putAddressStates [] _ = return ()
putAddressStates (address:rest) addressState = do
    putAddressState address addressState
    putAddressStates rest addressState


addToBalance::Address->Integer->ContextM ()
addToBalance address val = do
  addressState <- fromMaybe startingAddressState <$> getAddressState address
  putAddressState address addressState{ balance = balance addressState + fromIntegral val }

addNonce::Address->ContextM ()
addNonce address = do
  addressState <- fromMaybe startingAddressState <$> getAddressState address
  putAddressState address addressState{ addressStateNonce = addressStateNonce addressState + 1 }






  










