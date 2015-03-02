{-# LANGUAGE OverloadedStrings #-}

module Blockchain.DB.ModifyStateDB (
  putAddressStates,
  addToBalance,
  incrementNonce,
  pay
) where

import Control.Monad
import Control.Monad.IO.Class
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressState

--import Debug.Trace
import Blockchain.Debug

putAddressStates::[Address]->AddressState->ContextM ()
putAddressStates [] _ = return ()
putAddressStates (address:rest) addressState = do
    putAddressState address addressState
    putAddressStates rest addressState


addToBalance::Address->Integer->ContextM ()
addToBalance address val = do
  addressState <- getAddressState address
  putAddressState address addressState{ balance = balance addressState + fromIntegral val }

incrementNonce::Address->ContextM ()
incrementNonce address = do
  addressState <- getAddressState address
  putAddressState address addressState{ addressStateNonce = addressStateNonce addressState + 1 }

pay::String->Address->Address->Integer->ContextM Bool
pay description fromAddr toAddr val = do
  when debug $ do
    liftIO $ putStrLn $ "payment: from " ++ show (pretty fromAddr) ++ " to " ++ show (pretty toAddr) ++ ": " ++ show val ++ ", " ++ description
    fromAddressState <- getAddressState fromAddr
    liftIO $ putStrLn $ "from Funds: " ++ show (balance fromAddressState)
    toAddressState <- getAddressState toAddr
    liftIO $ putStrLn $ "to Funds: " ++ show (balance toAddressState)
    when (balance fromAddressState < val) $
       liftIO $ putStrLn "insufficient funds"

  if val == 0
    then return True
    else do
    fromAddressState <- getAddressState fromAddr
    if balance fromAddressState < val
      then return False
      else do
      addToBalance fromAddr (-val)
      addToBalance toAddr val
      return True




  










