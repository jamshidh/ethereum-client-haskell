{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Crypto.Hash.SHA3 as C
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal
import Data.Function
import Data.List
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import qualified Database.LevelDB as DB
import Network.Haskoin.Crypto hiding (Address)
import Network.Socket (socketToHandle)
import Numeric
import System.Directory
import System.Entropy
import System.IO

--remove this
--import Data.Time

import Network.Simple.TCP

import Address
import Block
import BlockChain
import Colors
import Constants
import EthDB
import Format
import ModifyStateDB
import RLP
import SampleTransaction
import SHA
--import Transaction
import Util
import Wire

import SampleTransactions

--import Debug.Trace


prvKey::PrvKey
Just prvKey = makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380
--Just prvKey = makePrvKey 0xd69bceff85f3bc2d0a13bcc43b7caf6bd54a21ad0c1997ae623739216710ca19 --cpp client prvKey
--6ccf6b5c33ae2017a6c76b8791ca61276a69ab8e --cpp coinbase


ethereumHeader::ByteString->Put
ethereumHeader payload = do
  putWord32be 0x22400891
  putWord32be $ fromIntegral $ B.length payload
  putByteString payload

sendCommand::Socket->ByteString->IO ()
sendCommand socket payload = do
  let theData2 = runPut $ ethereumHeader payload
  send socket $ B.concat $ BL.toChunks theData2

sendMessage::Socket->Message->IO ()
sendMessage socket msg = do
  putStrLn (green "msg>>>>>: " ++ format msg)
  sendCommand socket $ rlpSerialize $ wireMessage2Obj msg

getNextBlock::Block->UTCTime->IO Block
getNextBlock b ts = do
  let theCoinbase = prvKey2Address prvKey
  newStateRoot <- runResourceT $ do
    homeDir <- liftIO getHomeDirectory
    sdb <- DB.open (homeDir ++ stateDBPath) DB.defaultOptions {
      DB.createIfMissing=True, DB.cacheSize=1024}
    addToBalance sdb (stateRoot bd) theCoinbase (1500*finney)

  return $ Block{
               blockData=testGetNextBlockData newStateRoot,
               receiptTransactions=[],
               blockUncles=[]
             }
  where
    testGetNextBlockData::SHAPtr->BlockData
    testGetNextBlockData newStateRoot =
      BlockData {
        parentHash=blockHash b,
        unclesHash=hash $ B.pack [0xc0],
        coinbase=prvKey2Address prvKey,
        stateRoot = newStateRoot,
        transactionsTrie = 0,
        difficulty =
          if (round (utcTimeToPOSIXSeconds ts)) >=
             (round (utcTimeToPOSIXSeconds (timestamp bd)) + 42::Integer)
          then difficulty bd - difficulty bd `shiftR` 10
          else difficulty bd + difficulty bd `shiftR` 10,
        --20000000, --13269813,
        number = number bd + 1,
        minGasPrice = 10000000000000, --minGasPrice bd,
        gasLimit = max 125000 ((gasLimit bd * 1023 + gasUsed bd *6 `quot` 5) `quot` 1024),
        gasUsed = 0,
        timestamp = ts,  
        extraData = 0,
        nonce = SHA 5
        }
    bd = blockData b


submitNextBlock::Socket->Block->IO()
submitNextBlock socket b = do
        ts <- getCurrentTime
        newBlock <- getNextBlock b ts
        print newBlock
        n <- fastFindNonce newBlock

        print $ showHex (powFunc $ addNonceToBlock newBlock n) ""
        let theBytes = headerHashWithoutNonce newBlock `B.append` B.pack (integer2Bytes n)
        print $ format theBytes
        print $ format $ C.hash 256 theBytes
        let theNewBlock = addNonceToBlock newBlock n
        sendMessage socket $ Blocks [theNewBlock]
        addBlocks [theNewBlock]



handlePayload::Socket->B.ByteString->IO ()
handlePayload socket payload = do
  let rlpObject = rlpDeserialize payload
  let msg = obj2WireMessage rlpObject
  putStrLn (red "msg<<<<: " ++ format msg)
  case msg of
    Ping -> sendMessage socket Pong
    GetPeers -> do
      sendMessage socket $ Peers []
      sendMessage socket $ GetPeers
    Blocks blocks -> do
      addBlocks $ sortBy (compare `on` number . blockData) blocks
      case blocks of
        [b] -> submitNextBlock socket b
        _ -> requestNewBlocks socket
      
      --sendMessage socket $ Blocks [addNonceToBlock newBlock n]
    GetTransactions -> do
      sendMessage socket $ Transactions []
      sendMessage socket $ GetTransactions

    _-> return ()

getPayloads::[Word8]->[[Word8]]
getPayloads [] = []
getPayloads (0x22:0x40:0x08:0x91:s1:s2:s3:s4:remainder) =
  take payloadLength remainder:getPayloads (drop payloadLength remainder)
  where
    payloadLength = shift (fromIntegral s1) 24 + shift (fromIntegral s2) 16 + shift (fromIntegral s3) 8 + fromIntegral s4
getPayloads _ = error "Malformed data sent to getPayloads"

readAndOutput::Socket->IO()
readAndOutput socket = do
  h <- socketToHandle socket ReadWriteMode
  payloads <- BL.hGetContents h
  handleAllPayloads $ getPayloads $ BL.unpack payloads
  where
    handleAllPayloads [] = error "Server has closed the connection"
    handleAllPayloads (pl:rest) = do
      handlePayload socket $ B.pack pl
      handleAllPayloads rest

requestNewBlocks::Socket->IO ()
requestNewBlocks socket = do
  maybeBestBlockHash <- withBlockDB getBestBlockHash

  bestBlockHash <-
    case maybeBestBlockHash of
      Nothing -> do
        initializeBlockChain
        _ <- initializeStateDB
        return $ blockHash genesisBlock
      Just x -> return x

  putStrLn $ "Best block hash: " ++ format bestBlockHash

  sendMessage socket $ GetChain [bestBlockHash] 0x40

sendHello::Socket->IO ()
sendHello socket = do
  peerId <- getEntropy 64

  sendMessage socket $ Hello {
        version = 23,
        clientId = "Ethereum(G)/v0.6.4//linux/Haskell",
        capability = [ProvidesPeerDiscoveryService,
                      ProvidesTransactionRelayingService,
                      ProvidesBlockChainQueryingService],
        port = 30303,
        nodeId = fromIntegral $ byteString2Integer peerId

        }

main::IO ()    
main = connect "127.0.0.1" "30303" $ \(socket, _) -> do
--main = connect "192.168.0.2" "30303" $ \(socket, _) -> do
  putStrLn "Connected"
  sendHello socket

  signedTx <- withSource devURandom $ signTransaction prvKey simpleTX
  sendMessage socket $ Transactions [signedTx]

  requestNewBlocks socket

  readAndOutput socket
  

