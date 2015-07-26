
module Blockchain.Mining (
  nonceIsValid'
  ) where


import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Crypto.Hash.SHA3 as SHA3
import qualified Data.Array.IO as A
import qualified Data.Binary as Bin
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock.POSIX
import Data.Word

import Blockchain.Context
import Blockchain.Data.BlockDB
import Blockchain.Data.RLP
import Blockchain.ExtWord
--import Blockchain.SHA
import Blockchain.Util

--import Cache
import Constants
--import Dataset
import Hashimoto

--import Debug.Trace


word32Unpack::B.ByteString->[Word32]
word32Unpack s | B.null s = []
word32Unpack s | B.length s >= 4 = Bin.decode (BL.fromStrict $ B.take 4 s) : word32Unpack (B.drop 4 s)
word32Unpack _ = error "word32Unpack called for ByteString of length not a multiple of 4"

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




--------------------------
--Mining stuff

--used as part of the powFunc
noncelessBlockData2RLP::BlockData->RLPObject
noncelessBlockData2RLP bd =
  RLPArray [
      rlpEncode $ blockDataParentHash bd,
      rlpEncode $ blockDataUnclesHash bd,
      rlpEncode $ blockDataCoinbase bd,
      rlpEncode $ blockDataStateRoot bd,
      rlpEncode $ blockDataTransactionsRoot bd,
      rlpEncode $ blockDataReceiptsRoot bd,
      rlpEncode $ blockDataLogBloom bd,
      rlpEncode $ blockDataDifficulty bd,
      rlpEncode $ blockDataNumber bd,
      rlpEncode $ blockDataGasLimit bd,
      rlpEncode $ blockDataGasUsed bd,
      rlpEncode (round $ utcTimeToPOSIXSeconds $ blockDataTimestamp bd::Integer),
      rlpEncode $ blockDataExtraData bd
      ]

headerHashWithoutNonce::Block->B.ByteString
headerHashWithoutNonce b = SHA3.hash 256 $ rlpSerialize $ noncelessBlockData2RLP $ blockBlockData b

