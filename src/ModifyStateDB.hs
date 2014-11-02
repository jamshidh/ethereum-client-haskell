{-# LANGUAGE OverloadedStrings #-}

module ModifyStateDB (
                      initializeBlankStateDB,
                      initializeStateDB,
                      addToBalance,
                      transferEther,
                      addNewAccount,
                      addNonce
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Default
import Data.Functor
import Data.Maybe
import qualified Database.LevelDB as DB
import System.Directory

import Address
import AddressState
import Constants
import DBs
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
      contractRoot=0,
      codeHash=hash B.empty
      }

initializeBlankStateDB::String->ResourceT IO (DB.DB, SHAPtr)
initializeBlankStateDB dbPath = do
  db <- DB.open dbPath DB.defaultOptions{DB.createIfMissing=True}
  DB.put db def startingRoot B.empty
  return (db, SHAPtr startingRoot)

initializeStateDB::IO SHAPtr
initializeStateDB = do
    runResourceT $ do
      homeDir <- liftIO $ getHomeDirectory
      (db, startingRoot2) <- initializeBlankStateDB (homeDir ++ stateDBPath)

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

      putAddressStates db startingRoot2 addresses
        startingAddressState{balance=0x0100000000000000000000000000000000000000000000000000}

putAddressStates::DB.DB->SHAPtr->[Address]->AddressState->ResourceT IO SHAPtr
putAddressStates _ stateRoot [] _ = return stateRoot
putAddressStates db stateRoot (address:rest) addressState = do
    newStateRoot <- putAddressState db stateRoot address addressState
    putAddressStates db newStateRoot rest addressState


addToBalance::StateDB->SHAPtr->Address->Integer->ResourceT IO SHAPtr
addToBalance sdb stateRoot address val = do
  addressState <- fromMaybe startingAddressState <$> getAddressState sdb stateRoot address
  putAddressState sdb stateRoot address
    addressState{ balance = balance addressState + fromIntegral val }

transferEther::StateDB->SHAPtr->Address->Address->Integer->ResourceT IO SHAPtr
transferEther sdb sr fromAddress toAddress val = do
  sr2 <- addToBalance sdb sr fromAddress (-val)
  addToBalance sdb sr2 toAddress val


addNewAccount::StateDB->SHAPtr->Address->B.ByteString->ResourceT IO SHAPtr
addNewAccount sdb stateRoot address code = do
  let addressState = AddressState {addressStateNonce=0, balance=0, contractRoot=0, codeHash=hash code}
  putAddressState sdb stateRoot address addressState


addNonce::StateDB->SHAPtr->Address->ResourceT IO SHAPtr
addNonce sdb stateRoot address = do
  addressState <- fromMaybe startingAddressState <$> getAddressState sdb stateRoot address
  putAddressState sdb stateRoot address
    addressState{ addressStateNonce = addressStateNonce addressState + 1 }






  










