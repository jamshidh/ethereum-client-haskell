{-# LANGUAGE OverloadedStrings #-}

module Blockchain.DB.ModifyStateDB (
  addToBalance,
  incrementNonce,
  pay
) where

import Control.Monad
import Control.Monad.Trans
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressState

--import Debug.Trace

addToBalance::Address->Integer->ContextM Bool
addToBalance address val = do
  addressState <- lift $ getAddressState address
  let newVal = balance addressState + val
  if newVal < 0
    then return False
    else do
    lift $ putAddressState address addressState{balance = newVal}
    return True



incrementNonce::Address->ContextM ()
incrementNonce address = do
  addressState <- lift $ getAddressState address
  lift $ putAddressState address addressState{ addressStateNonce = addressStateNonce addressState + 1 }

pay::String->Address->Address->Integer->ContextM Bool
pay description fromAddr toAddr val = do
  debug <- isDebugEnabled
  when debug $ do
    liftIO $ putStrLn $ "payment: from " ++ show (pretty fromAddr) ++ " to " ++ show (pretty toAddr) ++ ": " ++ show val ++ ", " ++ description
    fromAddressState <- lift $ getAddressState fromAddr
    liftIO $ putStrLn $ "from Funds: " ++ show (balance fromAddressState)
    toAddressState <- lift $ getAddressState toAddr
    liftIO $ putStrLn $ "to Funds: " ++ show (balance toAddressState)
    when (balance fromAddressState < val) $
       liftIO $ putStrLn "insufficient funds"

  fromAddressState <- lift $ getAddressState fromAddr
  if balance fromAddressState < val
    then return False
    else do
    _ <- addToBalance fromAddr (-val)
    _ <- addToBalance toAddr val
    return True




  










