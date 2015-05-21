
module Blockchain.Mining (
--  nonceIsValid'
  ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.ByteString as B

import Blockchain.Context
import Blockchain.Data.BlockDB
import Blockchain.ExtWord
import Blockchain.Util
import Numeric

import Cache
import Constants
import Dataset
import Hashimoto

import Debug.Trace

powFunc'::Cache->Block->IO Integer
powFunc' cache b =
  --trace (show $ headerHashWithoutNonce b) $
  fmap (byteString2Integer . snd) $
  hashimoto
      (headerHashWithoutNonce b)
      (B.pack $ word64ToBytes $ blockDataNonce $ blockBlockData b)
      (fromInteger $ fullSize 0)
      -- (fromInteger . (calcDataset 0 !))
      (calcDatasetItem cache)


nonceIsValid'::Block->ContextM Bool
nonceIsValid' b = do
  cxt <- get

  val <- liftIO $ powFunc' (miningCache cxt) b

  liftIO $ putStrLn (showHex val "") 
  liftIO $ putStrLn (showHex (
                        val *
                        blockDataDifficulty (blockBlockData b)
                        ) "")


  return $ val * blockDataDifficulty (blockBlockData b) < (2::Integer)^(256::Integer)
