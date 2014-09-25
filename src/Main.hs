
module Main (
  main
  ) where

import Control.Monad
import Crypto.Hash.SHA3
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal
import Data.ByteString.Base16
import Data.Functor
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Word
import Data.String
import Network.Haskoin.Crypto
import Network.Haskoin.Internals hiding (Ping, Pong, version, Message)
import Numeric

import Network.Simple.TCP

import Address
import Format
import RLP
import SHA
import Transaction
import Wire

import Debug.Trace

command2Bytes::String->[Word8]
command2Bytes command = map c2w command ++ replicate (12 - length command) 0

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
  
getVarInt::Get Integer
getVarInt = do
  first <- getWord8
  case first of
    0xFD -> fromIntegral `fmap` getWord16le
    0xFE -> fromIntegral `fmap` getWord32le
    0xFF -> fromIntegral `fmap` getWord64le
    val -> return $ fromIntegral val

sendCommand::Socket->ByteString->IO ()
sendCommand socket payload = do
  let theData2 = runPut $ ethereumHeader payload
  send socket $ B.concat $ BL.toChunks theData2

sendMessage::Socket->Message->IO ()
sendMessage socket msg = sendCommand socket $ B.pack $ rlp2Bytes $ wireMessage2Obj msg
  



printData::Socket->[Word8]->IO [Word8]
printData socket (0x22:0x40:0x08:0x91:s1:s2:s3:s4:remainder) | payloadLength <= length remainder = do
  let (payload, nextRemainder) = splitAt payloadLength remainder
  --putStrLn ("Got something: " ++ show payload)
  let (rlpObject, remainder) = rlpSplit payload
  --putStrLn ("RLP: " ++ show rlpObject)
  let msg = obj2WireMessage rlpObject
  --putStrLn ("Message: " ++ show msg)
  putStrLn ("Message: " ++ format msg)
  --let payloadString = runGet (BL.pack $ take payloadLength remainder)
  case msg of
    Ping -> sendMessage socket Pong
    GetPeers -> do
      sendMessage socket $ Peers []
      sendMessage socket $ GetPeers
    GetTransactions -> do
      sendMessage socket $ Transactions []
      sendMessage socket $ GetTransactions
    _-> return ()
  printData socket $ drop payloadLength remainder
    where
      payloadLength = shift (fromIntegral s1) 24 + shift (fromIntegral s2) 16 + shift (fromIntegral s3) 8 + fromIntegral s4
printData socket incompleteData = return incompleteData

readAndOutput::Socket->[Word8]->IO()
readAndOutput socket remainingData = do
  response <- recv socket 100
  case response of
    Nothing -> error "Connection closed"
    Just theData -> do
      nextRemaining <- printData socket (remainingData ++ B.unpack theData)
      readAndOutput socket nextRemaining

ethereumHeader::ByteString->Put
ethereumHeader payload = do
  putWord32be 0x22400891
  putWord32be $ fromIntegral $ B.length payload
  putByteString payload

--main = connect "127.0.0.1" "41393" $ \(socket, remoteAddr) -> do
main = connect "127.0.0.1" "30303" $ \(socket, remoteAddr) -> do
--main = connect "54.76.56.74" "30303" $ \(socket, remoteAddr) -> do
  putStrLn "Connected"

  timestamp1 <- round `fmap` getPOSIXTime

  let msg = Hello {
        version = 23,
        clientId = "Ethereum(G)/v0.6.4//linux/Go",
        capability = [ProvidesPeerDiscoveryService,
                      ProvidesTransactionRelayingService,
                      ProvidesBlockChainQueryingService],
        port = 30303,
        nodeId = "Q\211(_#\141\158\211\163]G\168\DC1\241\215\221\GSo\224\182\227J\235\246\\\188eH$Zu\228\SYN\244\154\151qH\233\249C\t\147\157V\237\DC2\223XQ\191\242`>\186OG`\190 \230}\162o"

        }

  sendMessage socket $ msg

  let Just prvKey = makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380
  
  putStrLn $ "Using prvKey: " ++ show (prvKey2Address prvKey)

  let tx = Transaction {
         tNonce = 18,
         gasPrice = 0x9184e72a000,
         tGasLimit = 550,
         --tGasLimit = 1,
         to = Address 0, --0x5b42bd01ff7b368cd80a477cb1cf0d407e2b1cbe,
         value = 3,
         tInit = 0x600260005460206000f2,
         v = 0,
         r = 0,
         s = 0
         }

  signedTx <- withSource devURandom $
                   signTransaction prvKey tx


  sendMessage socket $ Transactions [signedTx]

  sendMessage socket $ GetChain [SHA 0x3281f4b79941b15e2fd78eb851fddee144cd7d5f8169beaf0b58fbba7fedea32] 0x100


  putStrLn "Transaction has been sent"

  readAndOutput socket []

--Transaction message bytes: f8-5e-12-f8-5b-80-86-09-18-4e-72-a0-00-82-02-27-80-03-8a-60-02-60-00-54-60-20-60-00-f2-1c-a0-09-63-39-24-4d-c4-93-bd-94-9e-92-6e-dc-2e-8e-d4-4d-97-26-d0-33-ff-67-00-30-69-3d-d1-09-5e-3a-61-a0-26-60-79-5e-0f-27-73-6f-5d-13-8a-db-2e-8d-0b-a6-18-ec-b7-57-fb-6c-0f-72-22-63-b4-9a-3c-0b-22-bd
