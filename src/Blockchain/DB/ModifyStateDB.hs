{-# LANGUAGE OverloadedStrings #-}

module Blockchain.DB.ModifyStateDB (
  addToBalance,
  pay
) where

import Control.Monad
import Control.Monad.Trans
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.DataDefs

--import Debug.Trace

addToBalance::Address->Integer->ContextM Bool
addToBalance address val = do
  addressState <- lift $ getAddressState address
  let newVal = addressStateBalance addressState + val
  if newVal < 0
    then return False
    else do
    lift $ putAddressState address addressState{addressStateBalance = newVal}
    return True

pay::String->Address->Address->Integer->ContextM Bool
pay description fromAddr toAddr val = do
  debug <- isDebugEnabled
  when debug $ do
    liftIO $ putStrLn $ "payment: from " ++ show (pretty fromAddr) ++ " to " ++ show (pretty toAddr) ++ ": " ++ show val ++ ", " ++ description
    fromAddressState <- lift $ getAddressState fromAddr
    liftIO $ putStrLn $ "from Funds: " ++ show (addressStateBalance fromAddressState)
    toAddressState <- lift $ getAddressState toAddr
    liftIO $ putStrLn $ "to Funds: " ++ show (addressStateBalance toAddressState)
    when (addressStateBalance fromAddressState < val) $
       liftIO $ putStrLn "insufficient funds"

  fromAddressState <- lift $ getAddressState fromAddr
  if addressStateBalance fromAddressState < val
    then return False
    else do
    _ <- addToBalance fromAddr (-val)
    _ <- addToBalance toAddr val
    return True




  










