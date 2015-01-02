{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
  ) where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
--import Data.Functor
import Data.Time.Clock
import Data.Word
import Network.Haskoin.Crypto hiding (Address)
import Network.Socket (socketToHandle)
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Entropy
import System.Environment
import System.IO

import Network.Simple.TCP

import Blockchain.Data.RLP

import Blockchain.BlockChain
import Blockchain.BlockSynchronizer
import Blockchain.Communication
import Blockchain.Constants
import Blockchain.Context
import Blockchain.Data.Address
--import Data.AddressState
import Blockchain.Data.Block
--import Data.SignedTransaction
--import Data.Transaction
import Blockchain.Data.Wire
import Blockchain.Database.MerklePatricia
import Blockchain.Display
import Blockchain.DB.ModifyStateDB
import Blockchain.PeerUrls
--import SampleTransactions
import Blockchain.SHA
import Blockchain.Util

--import Debug.Trace


prvKey::PrvKey
Just prvKey = makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380
--Just prvKey = makePrvKey 0xd69bceff85f3bc2d0a13bcc43b7caf6bd54a21ad0c1997ae623739216710ca19 --cpp client prvKey
--6ccf6b5c33ae2017a6c76b8791ca61276a69ab8e --cpp coinbase



getNextBlock::Block->UTCTime->ContextM Block
getNextBlock b ts = do
  let theCoinbase = prvKey2Address prvKey
  setStateRoot $ bStateRoot bd
  addToBalance theCoinbase (1500*finney)

  ctx <- get

  return Block{
               blockData=testGetNextBlockData $ stateRoot $ stateDB ctx,
               receiptTransactions=[],
               blockUncles=[]
             }
  where
    testGetNextBlockData::SHAPtr->BlockData
    testGetNextBlockData sr =
      BlockData {
        parentHash=blockHash b,
        unclesHash=hash $ B.pack [0xc0],
        coinbase=prvKey2Address prvKey,
        bStateRoot = sr,
        transactionsRoot = emptyTriePtr,
        receiptsRoot = emptyTriePtr,
        logBloom = B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],         
        difficulty = nextDifficulty (difficulty bd) (timestamp bd) ts,
        number = number bd + 1,
        gasLimit = max 125000 ((gasLimit bd * 1023 + gasUsed bd *6 `quot` 5) `quot` 1024),
        gasUsed = 0,
        timestamp = ts,  
        extraData = 0,
        nonce = SHA 5
        }
    bd = blockData b


submitNextBlock::Socket->Integer->Block->ContextM ()
submitNextBlock socket baseDifficulty b = do
        ts <- liftIO getCurrentTime
        newBlock <- getNextBlock b ts
        n <- liftIO $ fastFindNonce newBlock

        --let theBytes = headerHashWithoutNonce newBlock `B.append` B.pack (integer2Bytes n)
        let theNewBlock = addNonceToBlock newBlock n
        sendMessage socket $ NewBlockPacket theNewBlock (baseDifficulty + difficulty (blockData theNewBlock))
        addBlocks [theNewBlock]

ifBlockInDBSubmitNextBlock::Socket->Integer->Block->ContextM ()
ifBlockInDBSubmitNextBlock socket baseDifficulty b = do
  maybeBlock <- getBlock (blockHash b)
  case maybeBlock of
    Nothing -> return ()
    _ -> submitNextBlock socket baseDifficulty b



handlePayload::Socket->B.ByteString->ContextM ()
handlePayload socket payload = do
  --liftIO $ print $ payload
  --liftIO $ putStrLn $ show $ pretty $ rlpDeserialize payload
  let rlpObject = rlpDeserialize payload
  let msg = obj2WireMessage rlpObject
  displayMessage False msg
  case msg of
    Hello{} -> do
             bestBlock <- getBestBlock
             genesisBlockHash <- getGenesisBlockHash
             sendMessage socket Status{protocolVersion=fromIntegral ethVersion, networkID="", totalDifficulty=0, latestHash=blockHash bestBlock, genesisHash=genesisBlockHash}
    Ping -> do
      addPingCount
      sendMessage socket Pong
    GetPeers -> do
      sendMessage socket $ Peers []
      sendMessage socket GetPeers
    (Peers thePeers) -> do
      setPeers thePeers
    BlockHashes blockHashes -> handleNewBlockHashes socket blockHashes
    Blocks blocks -> do
      handleNewBlocks socket blocks
    NewBlockPacket block baseDifficulty -> do
      addBlocks [block]
      ifBlockInDBSubmitNextBlock socket baseDifficulty block

    Status{latestHash=lh} ->
        handleNewBlockHashes socket [lh]
    GetTransactions -> do
      sendMessage socket $ Transactions []
      --liftIO $ sendMessage socket GetTransactions
      return ()
      
    _-> return ()

getPayloads::[Word8]->[[Word8]]
getPayloads [] = []
getPayloads (0x22:0x40:0x08:0x91:s1:s2:s3:s4:remainder) =
  take payloadLength remainder:getPayloads (drop payloadLength remainder)
  where
    payloadLength = shift (fromIntegral s1) 24 + shift (fromIntegral s2) 16 + shift (fromIntegral s3) 8 + fromIntegral s4
getPayloads _ = error "Malformed data sent to getPayloads"

readAndOutput::Socket->ContextM ()
readAndOutput socket = do
  h <- liftIO $ socketToHandle socket ReadWriteMode
  payloads <- liftIO $ BL.hGetContents h
  handleAllPayloads $ getPayloads $ BL.unpack payloads
  where
    handleAllPayloads [] = error "Server has closed the connection"
    handleAllPayloads (pl:rest) = do
      handlePayload socket $ B.pack pl
      handleAllPayloads rest

mkHello::IO Message
mkHello = do
  peerId <- getEntropy 64
  return Hello {
               version = fromIntegral shhVersion,
               clientId = "Ethereum(G)/v0.6.4//linux/Haskell",
               capability = [ETH ethVersion, SHH shhVersion],
               port = 30303,
               nodeId = fromIntegral $ byteString2Integer peerId
             }

{-
createTransaction::Transaction->ContextM SignedTransaction
createTransaction t = do
    userNonce <- addressStateNonce <$> getAddressState (prvKey2Address prvKey)
    liftIO $ withSource devURandom $ signTransaction prvKey t{tNonce=userNonce}

createTransactions::[Transaction]->ContextM [SignedTransaction]
createTransactions transactions = do
    userNonce <- addressStateNonce <$> getAddressState (prvKey2Address prvKey)
    forM (zip transactions [userNonce..]) $ \(t, n) -> do
      liftIO $ withSource devURandom $ signTransaction prvKey t{tNonce=n}
-}

doit::Socket->ContextM ()
doit socket = do
  sendMessage socket =<< (liftIO mkHello)
  (setStateRoot . bStateRoot . blockData) =<< getBestBlock
  --signedTx <- createTransaction simpleTX
  --signedTx <- createTransaction outOfGasTX
  --signedTx <- createTransaction simpleStorageTX
  --signedTx <- createTransaction createContractTX
  --signedTx <- createTransaction sendMessageTX

  --signedTx <- createTransaction createContractTX
  --signedTx <- createTransaction paymentContract
  --signedTx <- createTransaction sendCoinTX
  --signedTx <- createTransaction keyValuePublisher
  --signedTx <- createTransaction sendKeyVal

  --liftIO $ print $ whoSignedThisTransaction signedTx

                
  --liftIO $ sendMessage socket $ Transactions [signedTx]


  --signedTxs <- createTransactions [createMysteryContract]
  --liftIO $ sendMessage socket $ Transactions signedTxs


  readAndOutput socket

main::IO ()    
main = do

  [ipNum] <- getArgs

  let (ipAddress, port') = ipAddresses !! read ipNum

  connect ipAddress port' $ \(socket, _) -> do
    putStrLn "Connected"

    runResourceT $ do
      cxt <- openDBs "h"
      _ <- liftIO $ runStateT (doit socket) cxt
      return ()
