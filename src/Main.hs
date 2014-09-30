
module Main (
  main
  ) where

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal
import Data.ByteString.Base16
import Data.Time.Clock
import Data.Functor
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Word
import Data.String
import Network.Haskoin.Crypto
import Network.Haskoin.Internals hiding (Ping, Pong, version, Message, Block, timestamp)
import Network.Socket (socketToHandle)
import Numeric
import System.IO

import Network.Simple.TCP

import Address
import Block
import Colors
import Format
import RLP
import SHA
import Transaction
import Util
import Wire

--import Debug.Trace

data IPAddress = IPV4Address Word8 Word8 Word8 Word8 |
  IPV6Address Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 

Just prvKey = makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380

address::IPAddress->Word16->Word64->Put
address (IPV4Address d1 d2 d3 d4) port flags = do
  putWord64le flags
  putByteString $ B.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF]
  putWord8 d1
  putWord8 d2
  putWord8 d3
  putWord8 d4
  putWord16le port
  
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
  sendCommand socket $ B.pack $ rlp2Bytes $ wireMessage2Obj msg


  
--testGetNextBlock::Block->UTCTime->Block
testGetNextBlock b ts =
  Block{
    blockData=testGetNextBlockData b ts,
    receiptTransactions=[],
    blockUncles=[]
    }
  where
    --testGetNextBlockData::Block->UTCTime->BlockData
    testGetNextBlockData b ts =
      BlockData {
        parentHash=blockHash b,
        unclesHash=hash $ B.pack [0xc0],
        coinbase=prvKey2Address prvKey,
        stateRoot = SHA 1,
        transactionsTrie = 0,
        difficulty = 1000000, --13269813, --difficulty $ blockData b,
        number = 0,
        minGasPrice = 10000000000000, --minGasPrice $ blockData b,
        gasLimit = 125000, --gasLimit $ blockData b,
        gasUsed = 0,
        timestamp = posixSecondsToUTCTime $ fromIntegral (1411763223::Integer), --ts,
        extraData = 0,
        nonce = SHA 5
        }







handlePayload::Socket->[Word8]->IO ()
handlePayload socket payload = do
  let (rlpObject, []) = rlpSplit payload
  let msg = obj2WireMessage rlpObject
  putStrLn (red "msg<<<<: " ++ format msg)
  case msg of
    Ping -> sendMessage socket Pong
    GetPeers -> do
      sendMessage socket $ Peers []
      sendMessage socket $ GetPeers
    Blocks [b] -> do
      ts <- getCurrentTime
      let newBlock = testGetNextBlock b ts
      nonce <- fastFindNonce newBlock
      sendMessage socket $ Blocks [addNonceToBlock newBlock nonce]
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

readAndOutput::Socket->IO()
readAndOutput socket = do
  h <- socketToHandle socket ReadWriteMode
  payloads <- BL.hGetContents h
  handleAllPayloads $ getPayloads $ BL.unpack payloads
  where
    handleAllPayloads [] = error "Server has closed the connection"
    handleAllPayloads (pl:rest) = do
      handlePayload socket pl
      handleAllPayloads rest

main = connect "127.0.0.1" "30303" $ \(socket, remoteAddr) -> do
  putStrLn "Connected"

  sendMessage socket $ Hello {
        version = 23,
        clientId = "Ethereum(G)/v0.6.4//linux/Go",
        capability = [ProvidesPeerDiscoveryService,
                      ProvidesTransactionRelayingService,
                      ProvidesBlockChainQueryingService],
        port = 30303,
        nodeId = fromIntegral $ byteString2Integer $ BC.pack "Q\211(_#\141\158\211\163]G\168\DC1\241\215\221\GSo\224\182\227J\235\246\\\188eH$Zu\228\SYN\244\154\151qH\233\249C\t\147\157V\237\DC2\223XQ\191\242`>\186OG`\190 \230}\162o"

        }

  let tx = Transaction {
         tNonce = 28,
         gasPrice = 0x9184e72a000,
         tGasLimit = 550,
         to = Address 0, --0x5b42bd01ff7b368cd80a477cb1cf0d407e2b1cbe,
         value = 3,
         tInit = 0x600260005460206000f2,
         v = 0,
         r = 0,
         s = 0
         }

  signedTx <- withSource devURandom $
                   signTransaction prvKey tx
  let b = Block{blockData=
                   BlockData {
                     parentHash=SHA 0,
                     unclesHash=hash $ B.pack [0xc0],
                     coinbase=prvKey2Address prvKey,
                     stateRoot = SHA 1,
                     transactionsTrie = 0,
                     difficulty = 13269813,
                     number = 0,
                     minGasPrice = 10000000000000,
                     gasLimit = 125000,
                     gasUsed = 0,
                     timestamp = posixSecondsToUTCTime $ fromIntegral (1411763223::Integer), --ts,
                     extraData = 0,
                     nonce = SHA 5
                     },
                receiptTransactions=[],
                blockUncles=[]
               }
  let ts = 0

  let newBlock = testGetNextBlock b ts
  --let powVal = byteString2Integer $ BC.pack $ powFunc newBlock
  --putStrLn $ "powFunc = " ++ show (showHex powVal "")
  --let passed = powVal * (difficulty $ blockData newBlock) < 2^256
  --putStrLn (red "Passed: " ++ show passed)
  --theNonce <- (fastFindNonce newBlock)::IO Integer
  --sendMessage socket $ Blocks [addNonceToBlock newBlock theNonce]


  --sendMessage socket $ Transactions [signedTx]
  sendMessage socket $ GetChain [blockHash genesisBlock] 0x40
  putStrLn "Transaction has been sent"

  readAndOutput socket

