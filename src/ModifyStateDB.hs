{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module ModifyStateDB (
                      initializeBlankStateDB,
                      initializeStateDB,
                      addReward
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Crypto.Hash.SHA3 as C
import Data.Binary
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Internal
import qualified Data.ByteString.Char8 as BC
import Data.Default
import Data.Function
import Data.Functor
import Data.List
import qualified Database.LevelDB as DB
import Numeric
import System.Directory

import Address
import AddressState
import Colors
import Constants
import EthDB
import Format
import qualified NibbleString as N
import Util
import RLP
import SHA

--import Debug.Trace

startingRoot::B.ByteString
(startingRoot, "") = B16.decode "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
                     --"bc36789e7a1e281436464229828f817d6612f7b477d66591ff96a9e064bcc98a"                                                                                                                   

startingAddressState =
      AddressState {
      addressStateNonce=0,
      balance= 0x0100000000000000000000000000000000000000000000000000,
      contractRoot=0,
      codeHash=hash B.empty
      }

initializeBlankStateDB::String->ResourceT IO (DB.DB, SHAPtr)
initializeBlankStateDB dbPath = do
  homeDir <- liftIO $ getHomeDirectory
  db <- DB.open dbPath DB.defaultOptions{DB.createIfMissing=True}
  DB.put db def startingRoot B.empty
  return (db, SHAPtr startingRoot)

initializeStateDB::IO ()
initializeStateDB = do
    runResourceT $ do
      homeDir <- liftIO $ getHomeDirectory
      (db, startingRoot) <- initializeBlankStateDB (homeDir ++ stateDBPath)

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

      newStateRoot <- putAddressStates db startingRoot addresses startingAddressState

      return ()

putAddressStates::DB.DB->SHAPtr->[Address]->AddressState->ResourceT IO SHAPtr
putAddressStates _ stateRoot [] _ = return stateRoot
putAddressStates db stateRoot (address:rest) addressState = do
    newStateRoot <- putAddressState db stateRoot address addressState
    putAddressStates db newStateRoot rest addressState


addReward::SHAPtr->Address->IO SHAPtr
addReward stateRoot address = 
    runResourceT $ do
      homeDir <- liftIO $ getHomeDirectory
      db <- DB.open (homeDir ++ "/" ++ stateDBPath) DB.defaultOptions{DB.createIfMissing=True}
      --DB.put db def startingRoot B.empty

      maybeAddressState <- getAddressState db stateRoot address

      case maybeAddressState of
        Nothing -> putAddressState db stateRoot address startingAddressState{ balance = fromIntegral $ 1500*finney }
        Just addressState -> putAddressState db stateRoot address (addressState{balance=balance addressState + fromIntegral (1500*finney)})

  






