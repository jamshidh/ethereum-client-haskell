
module Main (
  main
  ) where

import Control.Monad
import Crypto.Hash.SHA256
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal
import Data.Functor
import Data.Time.Clock.POSIX
import Data.Word
import Data.String
import Network.Haskoin.Crypto

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

main = connect "127.0.0.1" "44090" $ \(socket, remoteAddr) -> do
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


  sendCommand socket $ B.pack $ rlp2Bytes $
    RLPArray [RLPNumber 0x12,
              RLPArray[
                RLPNumber 0, --nonce
                RLPString "\t\CANNr\160\NUL", --gas
                RLPNumber 510,--gas
                RLPString "[B\189\SOH\255{6\140\216\nG|\177\207\r@~+\FS\190", --to
                RLPNumber 1,--amount
                RLPNumber 0,--init
                RLPNumber 27,
                RLPString "\137h`\187\153\205\209\169G\157\245\&9\187/\242(\222\NULQ\t\181Y\130R\226\134\170\230\167p\252\212",
                RLPString "}\147F\215\187\128\149\181\251\NAK-\128i\GS\176\143\244\229\ETX\223p\135\217fw^\203\162\137q\243\223"
                ]
             ]

  readAndOutput socket []


main2 = do
  {-
  print $ rlp2Bytes $
    wireMessage2Obj $
    Transactions [
      signTransaction 1 $
        Transaction {
          tNonce = 0,
          gasPrice = 1, -- "\t\CANNr\160\NUL", --gas
          tGasLimit = 510,
          to = "[B\189\SOH\255{6\140\216\nG|\177\207\r@~+\FS\190", --to,
          value = 1,
          tInit = 0,
          v = 27,
          r = "\137h`\187\153\205\209\169G\157\245\&9\187/\242(\222\NULQ\t\181Y\130R\226\134\170\230\167p\252\212",
          s = "}\147F\215\187\128\149\181\251\NAK-\128i\GS\176\143\244\229\ETX\223p\135\217fw^\203\162\137q\243\223"
          }
      ]
  -}

  let Just prvKey = makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380
  
  putStrLn $ format $ B.pack $ rlp2Bytes $
    wireMessage2Obj $
    Transactions [
      signTransaction prvKey $
        Transaction {
          tNonce = 0,
          gasPrice = 0x9184e72a000,
          tGasLimit = 510,
          to = 0x5b42bd01ff7b368cd80a477cb1cf0d407e2b1cbe,
          value = 1,
          tInit = 0,
          v = 0,
          r = "",
          s = ""
          }
      ]


  putStrLn $ format $ B.pack $ rlp2Bytes $
    wireMessage2Obj $
    Transactions [
      signTransaction prvKey $
        Transaction {
          tNonce = 0,
          gasPrice = 0x09184e72a000, --gas
          tGasLimit = 0x4255,
          to = 0x79b08ad8787060333663d19704909ee7b1903e58, --to,
          value = 1000000000000000000000,
          tInit = 0,
          v = 0,
          r = "",
          s = ""
          }
      ]

