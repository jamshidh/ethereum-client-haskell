{-# LANGUAGE OverloadedStrings #-}

module Blockchain.DB.ModifyStateDB (
  addToBalance,
  incrementNonce,
  pay
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.DataDefs
import Blockchain.Data.AddressStateDB

--import Debug.Trace
import Blockchain.Debug

addToBalance::Address->Integer->ContextM ()
addToBalance address val = do
  addressState <- lift $ getAddressState address
  lift $ putAddressState address addressState{ addressStateBalance = addressStateBalance addressState + fromIntegral val }

incrementNonce::Address->ContextM ()
incrementNonce address = do
  addressState <- lift $ getAddressState address
  lift $ putAddressState address addressState{ addressStateNonce = addressStateNonce addressState + 1 }

pay::String->Address->Address->Integer->ContextM Bool
pay description fromAddr toAddr val = do
  when debug $ do
    liftIO $ putStrLn $ "payment: from " ++ show (pretty fromAddr) ++ " to " ++ show (pretty toAddr) ++ ": " ++ show val ++ ", " ++ description
    fromAddressState <- lift $ getAddressState fromAddr
    liftIO $ putStrLn $ "from Funds: " ++ show (addressStateBalance fromAddressState)
    toAddressState <- lift $ getAddressState toAddr
    liftIO $ putStrLn $ "to Funds: " ++ show (addressStateBalance toAddressState)
    when (addressStateBalance fromAddressState < val) $
       liftIO $ putStrLn "insufficient funds"

  if val == 0
    then return True
    else do
    fromAddressState <- lift $ getAddressState fromAddr
    if addressStateBalance fromAddressState < val
      then return False
      else do
      addToBalance fromAddr (-val)
      addToBalance toAddr val
      return True




  










