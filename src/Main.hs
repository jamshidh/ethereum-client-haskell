
module Main (
  main
  ) where

import Control.Monad
import Crypto.Hash.SHA3
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal
import Data.ByteString.Base16
import Data.Functor
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Word
import Data.String
import Network.Haskoin.Crypto
import Network.Haskoin.Internals hiding (Ping, Pong, version, Message, Block)
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

  let Just prvKey = makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380
  
  let tx = Transaction {
         tNonce = 18,
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

  --sendMessage socket $ Transactions [signedTx]
  sendMessage socket $ GetChain [SHA 0x3281f4b79941b15e2fd78eb851fddee144cd7d5f8169beaf0b58fbba7fedea32] 0x40
  putStrLn "Transaction has been sent"

  readAndOutput socket

main2 = do
  let bytes = [248,183,248,179,160,11,245,9,22,218,43,135,138,4,114,225,163,235,236,138,158,246,62,195,180,171,97,188,180,50,201,205,215,68,229,224,97,160,29,204,77,232,222,199,93,122,171,133,181,103,182,204,212,26,211,18,69,27,148,138,116,19,240,161,66,253,64,212,147,71,148,108,207,107,92,51,174,32,23,166,199,107,135,145,202,97,39,106,105,171,142,160,101,198,63,17,219,190,89,68,9,114,151,18,156,75,239,89,209,217,248,148,97,89,7,72,152,87,59,163,198,99,112,47,128,131,199,167,60,130,71,64,134,9,24,78,114,160,0,131,1,232,72,128,132,84,36,177,229,128,160,208,75,195,104,225,120,86,175,0,223,4,187,59,241,126,101,151,241,13,241,113,141,80,4,196,189,251,177,41,74,40,72,192,192]
  print $ encode $ hash 256 $ B.pack bytes
  let (rlp, []) = rlpSplit bytes
  print rlp
  let block = (rlpDecode rlp)::Block
  let rlp2 = rlpEncode block
  let bytes2 = rlp2Bytes rlp2
  print rlp2
  print $ encode $ hash 256 $ B.pack bytes2

