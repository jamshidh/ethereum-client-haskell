{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
  ) where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.Time.Clock
import Data.Word
import Network
import Network.Haskoin.Crypto hiding (Address)
import System.Entropy
import System.Environment
import System.IO


import Blockchain.Data.RLP

import Blockchain.BlockChain
import Blockchain.BlockSynchronizer
import Blockchain.Communication
import Blockchain.Constants
import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressState
import Blockchain.Data.Block
import Blockchain.Data.SignedTransaction
import Blockchain.Data.Transaction
import Blockchain.Data.Wire
import Blockchain.Database.MerklePatricia
import Blockchain.DB.CodeDB
import Blockchain.DB.ModifyStateDB
import Blockchain.DBM
import Blockchain.Display
import Blockchain.PeerUrls
import Blockchain.SampleTransactions
import Blockchain.SHA
import Blockchain.SigningTools
import Blockchain.Util

--import Debug.Trace


prvKey::PrvKey
Just prvKey = makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380
--Just prvKey = makePrvKey 0xd69bceff85f3bc2d0a13bcc43b7caf6bd54a21ad0c1997ae623739216710ca19 --cpp client prvKey
--6ccf6b5c33ae2017a6c76b8791ca61276a69ab8e --cpp coinbase



getNextBlock::Block->UTCTime->ContextM Block
getNextBlock b ts = do
  let theCoinbase = prvKey2Address prvKey
  lift $ setStateRoot $ bStateRoot bd
  addToBalance theCoinbase (1500*finney)

  dbs <- lift get

  return Block{
               blockData=testGetNextBlockData $ stateRoot $ stateDB dbs,
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


submitNextBlock::Handle->Integer->Block->ContextM ()
submitNextBlock handle baseDifficulty b = do
        ts <- liftIO getCurrentTime
        newBlock <- getNextBlock b ts
        n <- liftIO $ fastFindNonce newBlock

        --let theBytes = headerHashWithoutNonce newBlock `B.append` B.pack (integer2Bytes n)
        let theNewBlock = addNonceToBlock newBlock n
        sendMessage handle $ NewBlockPacket theNewBlock (baseDifficulty + difficulty (blockData theNewBlock))
        addBlocks [theNewBlock]

ifBlockInDBSubmitNextBlock::Handle->Integer->Block->ContextM ()
ifBlockInDBSubmitNextBlock handle baseDifficulty b = do
  maybeBlock <- lift $ getBlock (blockHash b)
  case maybeBlock of
    Nothing -> return ()
    _ -> submitNextBlock handle baseDifficulty b



handlePayload::Handle->B.ByteString->ContextM ()
handlePayload handle payload = do
  --liftIO $ print $ payload
  --liftIO $ putStrLn $ show $ pretty $ rlpDeserialize payload
  let rlpObject = rlpDeserialize payload
  let msg = obj2WireMessage rlpObject
  displayMessage False msg
  case msg of
    Hello{} -> do
             bestBlock <- getBestBlock
             genesisBlockHash <- getGenesisBlockHash
             sendMessage handle Status{protocolVersion=fromIntegral ethVersion, networkID="", totalDifficulty=0, latestHash=blockHash bestBlock, genesisHash=genesisBlockHash}
    Ping -> do
      addPingCount
      sendMessage handle Pong
    GetPeers -> do
      sendMessage handle $ Peers []
      sendMessage handle GetPeers
    (Peers thePeers) -> do
      setPeers thePeers
    BlockHashes blockHashes -> handleNewBlockHashes handle blockHashes
    Blocks blocks -> do
      handleNewBlocks handle blocks
    NewBlockPacket block baseDifficulty -> do
      addBlocks [block]
      ifBlockInDBSubmitNextBlock handle baseDifficulty block

    Status{latestHash=lh, genesisHash=gh} -> do
      genesisBlockHash <- getGenesisBlockHash
      when (gh /= genesisBlockHash) $ error "Wrong genesis block hash!!!!!!!!"
      handleNewBlockHashes handle [lh]
    GetTransactions -> do
      sendMessage handle $ Transactions []
      --liftIO $ sendMessage handle GetTransactions
      return ()
      
    _-> return ()

getPayloads::[Word8]->[[Word8]]
getPayloads [] = []
getPayloads (0x22:0x40:0x08:0x91:s1:s2:s3:s4:remainder) =
  take payloadLength remainder:getPayloads (drop payloadLength remainder)
  where
    payloadLength = shift (fromIntegral s1) 24 + shift (fromIntegral s2) 16 + shift (fromIntegral s3) 8 + fromIntegral s4
getPayloads _ = error "Malformed data sent to getPayloads"

readAndOutput::Handle->ContextM ()
readAndOutput h = do
  payloads <- liftIO $ BL.hGetContents h
  handleAllPayloads h $ getPayloads $ BL.unpack payloads
  where
    handleAllPayloads _ [] = error "Server has closed the connection"
    handleAllPayloads h (pl:rest) = do
      handlePayload h $ B.pack pl
      handleAllPayloads h rest

mkHello::IO Message
mkHello = do
  peerId <- getEntropy 64
  return Hello {
               version = 3,
               clientId = "Ethereum(G)/v0.6.4//linux/Haskell",
               capability = [ETH ethVersion, SHH shhVersion],
               port = 30303,
               nodeId = fromIntegral $ byteString2Integer peerId
             }

createTransaction::Transaction->ContextM SignedTransaction
createTransaction t = do
    userNonce <- lift $ addressStateNonce <$> getAddressState (prvKey2Address prvKey)
    liftIO $ withSource devURandom $ signTransaction prvKey t{tNonce=userNonce}

createTransactions::[Transaction]->ContextM [SignedTransaction]
createTransactions transactions = do
    userNonce <- lift $ addressStateNonce <$> getAddressState (prvKey2Address prvKey)
    forM (zip transactions [userNonce..]) $ \(t, n) -> do
      liftIO $ withSource devURandom $ signTransaction prvKey t{tNonce=n}

doit::Handle->ContextM ()
doit handle = do
  lift $ addCode B.empty --This is probably a bad place to do this, but I can't think of a more natural place to do it....  Empty code is used all over the place, and it needs to be in the database.
  sendMessage handle =<< (liftIO mkHello)
  lift . setStateRoot . bStateRoot . blockData =<< getBestBlock

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

                
  --sendMessage socket $ Transactions [signedTx]

  --signedTxs <- createTransactions [createMysteryContract]
  --liftIO $ sendMessage socket $ Transactions signedTxs


  readAndOutput handle

main::IO ()    
main = do

  args <- getArgs

  let ipNum =
        case args of
          (arg:_) -> arg
          [] -> "1" --Just default to poc-8.ethdev.com

  let (ipAddress, port) = ipAddresses !! read ipNum

  handle <- connectTo ipAddress (PortNumber port)

  putStrLn "Connected"

  runResourceT $ do
    cxt <- openDBs "h"
    _ <- liftIO $ runStateT (runStateT (doit handle) (Context [] 0 [])) cxt
    return ()
