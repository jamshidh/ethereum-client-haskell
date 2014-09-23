
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
import Network.Haskoin.Internals hiding (Ping, version)
import Numeric

import Network.Simple.TCP

import Format
import RLP
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
    Ping -> sendCommand socket $ B.pack $ rlp2Bytes $ RLPArray [RLPNumber 0x03]
    GetPeers -> do
      sendCommand socket $ B.pack $ rlp2Bytes $ RLPArray [RLPNumber 0x11]
      sendCommand socket $ B.pack $ rlp2Bytes $ RLPArray [RLPNumber 0x10]
    GetTransactions -> do
      sendCommand socket $ B.pack $ rlp2Bytes $ RLPArray [RLPNumber 0x12]
      sendCommand socket $ B.pack $ rlp2Bytes $ RLPArray [RLPNumber 0x16]
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

transaction = [
  --0x22, 0x40, 0x08, 0x91, 0x00, 0x00, 0x00, 0x6a,
  0xf8, 0x68, 0x12, 0xf8, 0x65, 0x80, 0x86, 0x09,
  0x18, 0x4e, 0x72, 0xa0, 0x00, 0x82, 0x01, 0xfe,
  0x94, 0x5b, 0x42, 0xbd, 0x01, 0xff, 0x7b, 0x36,
  0x8c, 0xd8, 0x0a, 0x47, 0x7c, 0xb1, 0xcf, 0x0d,
  0x40, 0x7e, 0x2b, 0x1c, 0xbe, 0x01, 0x80, 0x1b,
  0xa0, 0x89, 0x68, 0x60, 0xbb, 0x99, 0xcd, 0xd1,
  0xa9, 0x47, 0x9d, 0xf5, 0x39, 0xbb, 0x2f, 0xf2,
  0x28, 0xde, 0x00, 0x51, 0x09, 0xb5, 0x59, 0x82,
  0x52, 0xe2, 0x86, 0xaa, 0xe6, 0xa7, 0x70, 0xfc,
  0xd4, 0xa0, 0x7d, 0x93, 0x46, 0xd7, 0xbb, 0x80,
  0x95, 0xb5, 0xfb, 0x15, 0x2d, 0x80, 0x69, 0x1d,
  0xb0, 0x8f, 0xf4, 0xe5, 0x03, 0xdf, 0x70, 0x87,
  0xd9, 0x66, 0x77, 0x5e, 0xcb, 0xa2, 0x89, 0x71,
  0xf3, 0xdf]

getAddress prvKey =
  let PubKey point = derivePubKey prvKey in
  drop 24 $ concat $ map (flip showHex "") $ B.unpack $ hash 256 $ fst $ decode $ B.pack $ map c2w $ showHex (fromJust $ getX point) "" ++ showHex (fromJust $ getY point) ""


main = connect "127.0.0.1" "41393" $ \(socket, remoteAddr) -> do
--main = connect "54.72.69.180" "30303" $ \(socket, remoteAddr) -> do
--main = connect "54.76.56.74" "30303" $ \(socket, remoteAddr) -> do
  putStrLn "Connected"

  putStrLn $ show $ rlpSplit transaction

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

  let obj = wireMessage2Obj msg
  
  let payload = rlp2Bytes obj

  let (obj', remainder) = rlpSplit payload
  putStrLn ("obj: " ++ show obj')
  putStrLn ("remainder: " ++ show remainder)

  sendCommand socket $ B.pack payload

  let Just prvKey = makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380
  
  print $ getAddress prvKey

  let tx = Transaction {
         tNonce = 7,
         gasPrice = 0x9184e72a000,
         tGasLimit = 510,
         --tGasLimit = 1,
         to = 0x5b42bd01ff7b368cd80a477cb1cf0d407e2b1cbe,
         value = 3,
         tInit = 0,
         v = 0,
         r = 0,
         s = 0
         }

  signedTx <- withSource devURandom $
                   signTransaction prvKey tx

  let commandBytes = 
       B.pack $ rlp2Bytes $ 
           wireMessage2Obj $
               Transactions [signedTx]

  putStrLn $ format commandBytes

  sendCommand socket commandBytes


  readAndOutput socket []

