{-# LANGUAGE OverloadedStrings #-}

module Blockchain.DB.ModifyStateDB (
  putAddressStates,
  addToBalance,
  addNonce,
  pay
) where

--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressState

--import Debug.Trace

putAddressStates::[Address]->AddressState->ContextM ()
putAddressStates [] _ = return ()
putAddressStates (address:rest) addressState = do
    putAddressState address addressState
    putAddressStates rest addressState


addToBalance::Address->Integer->ContextM ()
addToBalance address val = do
  addressState <- getAddressState address
  putAddressState address addressState{ balance = balance addressState + fromIntegral val }

addNonce::Address->ContextM ()
addNonce address = do
  addressState <- getAddressState address
  putAddressState address addressState{ addressStateNonce = addressStateNonce addressState + 1 }

pay::Address->Address->Integer->ContextM ()
pay fromAddr toAddr val = do
  addToBalance fromAddr (-val)
  addToBalance toAddr val





  










