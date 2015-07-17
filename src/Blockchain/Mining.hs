
module Blockchain.Mining (
  nonceIsValid'
  ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.Array.IO as A
import qualified Data.Binary as Bin
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word

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


word32Unpack::B.ByteString->[Word32]
word32Unpack s | B.null s = []
word32Unpack s | B.length s >= 4 = Bin.decode (BL.fromStrict $ B.take 4 s) : word32Unpack (B.drop 4 s)
word32Unpack s = error "word32Unpack called for ByteString of length not a multiple of 4"

powFunc'::B.ByteString->Block->IO Integer
powFunc' dataset b = 
  --trace (show $ headerHashWithoutNonce b) $
  fmap (byteString2Integer . snd) $
  hashimoto
      (headerHashWithoutNonce b)
      (B.pack $ word64ToBytes $ blockDataNonce $ blockBlockData b)
      (fromInteger $ fullSize 0)
      -- (fromInteger . (calcDataset 0 !))
      (A.newListArray (0,15) . word32Unpack . B.take 64 . (flip B.drop dataset) . (64 *) . fromIntegral)

nonceIsValid'::Block->ContextM Bool
nonceIsValid' b = do
  cxt <- get

  val <- liftIO $ powFunc' (miningDataset cxt) b

{-
  liftIO $ putStrLn (showHex val "") 
  liftIO $ putStrLn (showHex (
                        val *
                        blockDataDifficulty (blockBlockData b)
                        ) "")
-}

  return $ val * blockDataDifficulty (blockBlockData b) < (2::Integer)^(256::Integer)
