{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blockchain.Verifier (
  checkValidity,
  isNonceValid,
  nextDifficulty
  ) where

import Control.Monad
import Control.Monad.IfElse
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.State hiding (state)
import Data.Binary hiding (get)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock
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
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.Data.TransactionResult
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.DB.CodeDB
import Blockchain.DB.ModifyStateDB
import Blockchain.DBM
import Blockchain.Constants
import Blockchain.ExtDBs
import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.Mining
import Blockchain.Options
import Blockchain.SHA
import Blockchain.Util
import Blockchain.VM
import Blockchain.VM.Code
import Blockchain.VM.OpcodePrices
import Blockchain.VM.VMState

--import Debug.Trace

{-
initializeBlockChain::ContextM ()
initializeBlockChain = do
  let bytes = rlpSerialize $ rlpEncode genesisBlock
  blockDBPut (BL.toStrict $ encode $ blockHash $ genesisBlock) bytes
  detailsDBPut "best" (BL.toStrict $ encode $ blockHash genesisBlock)
-}

nextDifficulty::Integer->UTCTime->UTCTime->Integer
nextDifficulty oldDifficulty oldTime newTime = max nextDiff' minimumDifficulty
    where
      nextDiff' = 
          if round (utcTimeToPOSIXSeconds newTime) >=
                 (round (utcTimeToPOSIXSeconds oldTime) + 8::Integer)
          then oldDifficulty - oldDifficulty `shiftR` 11
          else oldDifficulty + oldDifficulty `shiftR` 11

nextGasLimit::Integer->Integer->Integer
nextGasLimit oldGasLimit oldGasUsed = max (max 125000 3141592) ((oldGasLimit * 1023 + oldGasUsed *6 `quot` 5) `quot` 1024)

nextGasLimitDelta::Integer->Integer
nextGasLimitDelta oldGasLimit  = oldGasLimit `div` 1024

minGasLimit::Integer
minGasLimit = 125000

checkUnclesHash::Block->Bool
checkUnclesHash b = blockDataUnclesHash (blockBlockData b) == hash (rlpSerialize $ RLPArray (rlpEncode <$> blockBlockUncles b))

--data BlockValidityError = BlockDifficultyWrong Integer Integer | BlockNumberWrong Integer Integer | BlockGasLimitWrong Integer Integer | BlockNonceWrong | BlockUnclesHashWrong
{-
instance Format BlockValidityError where
    --format BlockOK = "Block is valid"
    format (BlockDifficultyWrong d expected) = "Block difficulty is wrong, is '" ++ show d ++ "', expected '" ++ show expected ++ "'"
-}

verifyStateRootExists::Block->ContextM Bool
verifyStateRootExists b = do
  val <- stateDBGet (BL.toStrict $ encode $ blockDataStateRoot $ blockBlockData b)
  case val of
    Nothing -> return False
    Just _ -> return True

checkParentChildValidity::(Monad m)=>Block->Block->m ()
checkParentChildValidity Block{blockBlockData=c} Block{blockBlockData=p} = do
    unless (blockDataDifficulty c == nextDifficulty (blockDataDifficulty p) (blockDataTimestamp p) (blockDataTimestamp c))
             $ fail $ "Block difficulty is wrong: got '" ++ show (blockDataDifficulty c) ++ "', expected '" ++ show (nextDifficulty (blockDataDifficulty p) (blockDataTimestamp p) (blockDataTimestamp c)) ++ "'"
    unless (blockDataNumber c == blockDataNumber p + 1) 
             $ fail $ "Block number is wrong: got '" ++ show (blockDataNumber c) ++ ", expected '" ++ show (blockDataNumber p + 1) ++ "'"
    unless (blockDataGasLimit c <= blockDataGasLimit p +  nextGasLimitDelta (blockDataGasLimit p))
             $ fail $ "Block gasLimit is too high: got '" ++ show (blockDataGasLimit c) ++ "', should be less than '" ++ show (blockDataGasLimit p +  nextGasLimitDelta (blockDataGasLimit p)) ++ "'"
    unless (blockDataGasLimit c >= blockDataGasLimit p - nextGasLimitDelta (blockDataGasLimit p))
             $ fail $ "Block gasLimit is too low: got '" ++ show (blockDataGasLimit c) ++ "', should be less than '" ++ show (blockDataGasLimit p -  nextGasLimitDelta (blockDataGasLimit p)) ++ "'"
    unless (blockDataGasLimit c >= minGasLimit)
             $ fail $ "Block gasLimit is lower than minGasLimit: got '" ++ show (blockDataGasLimit c) ++ "'"
    return ()

checkValidity::Monad m=>Block->ContextM (m ())
checkValidity b = do
  maybeParentBlock <- getBlock (blockDataParentHash $ blockBlockData b)
  case maybeParentBlock of
    Just parentBlock -> do
          checkParentChildValidity b parentBlock
          nIsValid <- nonceIsValid' b
          --unless nIsValid $ fail $ "Block nonce is wrong: " ++ format b
          unless (checkUnclesHash b) $ fail "Block unclesHash is wrong"
          stateRootExists <- verifyStateRootExists b
          unless stateRootExists $ fail ("Block stateRoot does not exist: " ++ show (pretty $ blockDataStateRoot $ blockBlockData b))
          return $ return ()
    Nothing -> fail ("Parent Block does not exist: " ++ show (pretty $ blockDataParentHash $ blockBlockData b))


{-
                    coinbase=prvKey2Address prvKey,
        stateRoot = SHA 0x9b109189563315bfeb13d4bfd841b129ff3fd5c85f228a8d9d8563b4dde8432e,
                    transactionsTrie = 0,
-}




isNonceValid::Transaction->ContextM Bool
isNonceValid t = do
  case whoSignedThisTransaction t of
    Nothing -> return False --no nonce would work
    Just tAddr -> do
      addressState <- getAddressState tAddr
      return $ addressStateNonce addressState == transactionNonce t

codeOrDataLength::Transaction->Int
codeOrDataLength t | isMessageTX t = B.length $ transactionData t
codeOrDataLength t | isContractCreationTX t = codeLength $ transactionInit t

zeroBytesLength::Transaction->Int
zeroBytesLength t | isMessageTX t = length $ filter (==0) $ B.unpack $ transactionData t
zeroBytesLength t | isContractCreationTX t = length $ filter (==0) $ B.unpack codeBytes
                  where
                    Code codeBytes = transactionInit t

intrinsicGas::Transaction->Integer
intrinsicGas t = gTXDATAZERO * zeroLen + gTXDATANONZERO * (fromIntegral (codeOrDataLength t) - zeroLen) + gTX
    where
      zeroLen = fromIntegral $ zeroBytesLength t
--intrinsicGas t@ContractCreationTX{} = 5 * (fromIntegral (codeOrDataLength t)) + 500

printTransactionMessage::Transaction->ContextM ()
printTransactionMessage t = do
  case whoSignedThisTransaction t of
    Just tAddr -> do
      nonce <- fmap addressStateNonce $ getAddressState tAddr
      liftIO $ putStrLn $ CL.magenta "    =========================================================================="
      liftIO $ putStrLn $ CL.magenta "    | Adding transaction signed by: " ++ show (pretty tAddr) ++ CL.magenta " |"
      liftIO $ putStrLn $ CL.magenta "    |    " ++
        (
          if isMessageTX t
          then "MessageTX to " ++ show (pretty $ transactionTo t) ++ "              "
          else "Create Contract "  ++ show (pretty $ getNewAddress_unsafe tAddr nonce)
        ) ++ CL.magenta " |"
    _ -> liftIO $ putStrLn $ CL.red $ "Malformed Signature!"

formatAddress::Address->String
formatAddress (Address x) = BC.unpack $ B16.encode $ B.pack $ word160ToBytes x


getBestBlockHash::ContextM SHA
getBestBlockHash = do
  maybeBestHash <- detailsDBGet "best"
  case maybeBestHash of
    Nothing -> do
      bhSHA <- getGenesisBlockHash
      detailsDBPut "best" $ BL.toStrict $ encode bhSHA
      return bhSHA
    Just bestHash -> return $ decode $ BL.fromStrict $ bestHash

getGenesisBlockHash::ContextM SHA
getGenesisBlockHash = do
  maybeGenesisHash <- detailsDBGet "genesis"
  case maybeGenesisHash of
    Nothing -> do
      bhSHA <- blockHash <$> initializeGenesisBlock
      detailsDBPut "genesis" $ BL.toStrict $ encode bhSHA
      return bhSHA
    Just bestHash -> return $ decode $ BL.fromStrict $ bestHash

getBestBlock::ContextM Block
getBestBlock = do
  bestBlockHash <- getBestBlockHash
  bestBlock <- getBlock bestBlockHash
  return $ fromMaybe (error $ "Missing block in database: " ++ show (pretty bestBlockHash)) bestBlock
      

replaceBestIfBetter::(BlockDataRefId, Block)->ContextM ()
replaceBestIfBetter (blkDataId, b) = do
  best <- getBestBlock
  if blockDataNumber (blockBlockData best) >= n
    then return ()
    else do
    detailsDBPut "best" (BL.toStrict $ encode $ blockHash b)
    let oldStateRoot = blockDataStateRoot (blockBlockData best)
        newStateRoot = blockDataStateRoot (blockBlockData b)
    sqlDiff blkDataId n oldStateRoot newStateRoot
  where n = blockDataNumber (blockBlockData b)
