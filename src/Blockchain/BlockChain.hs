{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blockchain.BlockChain (
  addBlocks
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Binary hiding (get)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Printf

import qualified Blockchain.Colors as CL
import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.Data.BlockDB
import Blockchain.Data.Code
import Blockchain.Data.DataDefs
import Blockchain.Data.DiffDB
import Blockchain.Data.GenesisBlock
import Blockchain.Data.Transaction
import Blockchain.Data.TransactionResult
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.ModifyStateDB
import Blockchain.DBM
import Blockchain.Constants
import Blockchain.ExtDBs
import Blockchain.ExtWord
import Blockchain.Options
import Blockchain.SHA

--import Debug.Trace

addBlocks::Bool->[Block]->ContextM ()
addBlocks isBeingCreated blocks = 
  forM_ blocks $ \block -> do
    before <- liftIO $ getPOSIXTime 
    putBlock block
    after <- liftIO $ getPOSIXTime 

    liftIO $ putStrLn $ "#### Block insertion time = " ++ printf "%.4f" (realToFrac $ after - before::Double) ++ "s"

