
module Blockchain.Mining (
--  nonceIsValid'
  ) where


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

{-
powFunc'::Cache->Block->Integer
powFunc' cache b =
  --trace (show $ headerHashWithoutNonce b) $
  byteString2Integer $
  snd $
  hashimoto
      (headerHashWithoutNonce b)
      (B.pack $ word64ToBytes $ blockDataNonce $ blockBlockData b)
      (fromInteger $ fullSize 0)
      -- (fromInteger . (calcDataset 0 !))
      (calcDatasetItem cache)


nonceIsValid'::Block->ContextM Bool
nonceIsValid' b = do
  cxt <- get
  trace (showHex (powFunc' (miningCache cxt) b) "") $ 
    return $
    powFunc' (miningCache cxt) b * blockDataDifficulty (blockBlockData b) < (2::Integer)^(256::Integer)
-}
