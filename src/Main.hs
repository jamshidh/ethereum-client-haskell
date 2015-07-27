{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import Control.Monad.Trans.State
import Control.Monad.Trans.Resource
import HFlags

import Blockchain.BlockChain
import Blockchain.Context
import Blockchain.DBM

main = do
  args <- $initHFlags "The Ethereum Haskell Peer"

  runResourceT $ do
      dbs <- openDBs "h"
      _ <- flip runStateT (Context
                           (stateDB' dbs)
                           (hashDB' dbs)
                           (blockDB' dbs)
                           (codeDB' dbs)
                           (sqlDB' dbs)
                           (detailsDB' dbs)
                           []) $
              addBlock undefined undefined


      return ()
