{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
  ) where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import qualified Crypto.Hash.SHA3 as C
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.Time.Clock
import Data.Word
import Network.Haskoin.Crypto hiding (Address)
import Network.Socket (socketToHandle)
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import System.Entropy
import System.IO

import Network.Simple.TCP

import Data.RLP

import BlockChain
import BlockSynchronizer
import qualified Colors as CL
import Constants
import Context
import Data.Address
import Data.AddressState
import Data.Block
import Data.SignedTransaction
import Data.Transaction
import Data.Wire
import Database.MerklePatricia
import Format
import DB.ModifyStateDB
import SampleTransactions
import SHA
import Util

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
        transactionsTrie = 0,
        difficulty = nextDifficulty (difficulty bd) (timestamp bd) ts,
        number = number bd + 1,
        minGasPrice = 10000000000000, --minGasPrice bd,
        gasLimit = max 125000 ((gasLimit bd * 1023 + gasUsed bd *6 `quot` 5) `quot` 1024),
        gasUsed = 0,
        timestamp = ts,  
        extraData = 0,
        nonce = SHA 5
        }
    bd = blockData b


submitNextBlock::Socket->Block->ContextM ()
submitNextBlock socket b = do
        ts <- liftIO getCurrentTime
        newBlock <- getNextBlock b ts
        liftIO $ print newBlock
        n <- liftIO $ fastFindNonce newBlock

        liftIO $ print $ showHex (powFunc $ addNonceToBlock newBlock n) ""
        let theBytes = headerHashWithoutNonce newBlock `B.append` B.pack (integer2Bytes n)
        liftIO $ print $ format theBytes
        liftIO $ print $ format $ C.hash 256 theBytes
        let theNewBlock = addNonceToBlock newBlock n
        liftIO $ sendMessage socket $ Blocks [theNewBlock]
        addBlocks [theNewBlock]

ifBlockInDBSubmitNextBlock::Socket->Block->ContextM ()
ifBlockInDBSubmitNextBlock socket b = do
  maybeBlock <- getBlock (blockHash b)
  case maybeBlock of
    Nothing -> return ()
    _ -> submitNextBlock socket b

handlePayload::Socket->B.ByteString->ContextM ()
handlePayload socket payload = do
  let rlpObject = rlpDeserialize payload
  let msg = obj2WireMessage rlpObject
  liftIO $ putStrLn (CL.red "msg<<<<: " ++ format msg)
  case msg of
    Ping -> liftIO $ sendMessage socket Pong
    GetPeers -> do
      liftIO $ sendMessage socket $ Peers []
      liftIO $ sendMessage socket GetPeers
    BlockHashes blockHashes -> handleNewBlockHashes socket blockHashes
    Blocks blocks -> do
      handleNewBlocks socket blocks
      case blocks of
        [] -> return ()
        [b] -> ifBlockInDBSubmitNextBlock socket b
        _ -> return () -- requestNewBlocks socket

    Status{latestHash=lh} ->
        requestNewBlocks socket lh

      --sendMessage socket $ Blocks [addNonceToBlock newBlock n]
    GetTransactions -> do
      liftIO $ sendMessage socket $ Transactions []
      liftIO $ sendMessage socket GetTransactions

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

requestNewBlocks::Socket->SHA->ContextM ()
requestNewBlocks socket latestBlockSHA = do
  --liftIO $ sendMessage socket $ GetBlockHashes [latestBlockSHA] 0x1000
  handleNewBlockHashes socket [latestBlockSHA]

mkHello::IO Message
mkHello = do
  peerId <- getEntropy 64
  return Hello {
               version = 0,
               clientId = "Ethereum(G)/v0.6.4//linux/Haskell",
               capability = [ETH, SHH],
               port = 30303,
               nodeId = fromIntegral $ byteString2Integer peerId
             }


createTransaction::Transaction->ContextM SignedTransaction
createTransaction t = do
    userNonce <- addressStateNonce <$> getAddressState (prvKey2Address prvKey)
    liftIO $ withSource devURandom $ signTransaction prvKey t{tNonce=userNonce}

doit::Socket->ContextM ()
doit socket = do
  (setStateRoot . bStateRoot . blockData) =<< getBestBlock
  --requestNewBlocks socket
  --signedTx <- createTransaction createContractTX
  --signedTx <- createTransaction paymentContract
  --signedTx <- createTransaction sendCoinTX
  --signedTx <- createTransaction keyValuePublisher
  signedTx <- createTransaction sendKeyVal

  liftIO $ print $ whoSignedThisTransaction signedTx

                
  liftIO $ sendMessage socket $ Transactions [signedTx]

  readAndOutput socket

  

main::IO ()    

--main = connect "185.43.109.23" "30303" $ \(socket, _) -> do
main = connect "127.0.0.1" "30303" $ \(socket, _) -> do
--main = connect "192.168.0.2" "30303" $ \(socket, _) -> do
  putStrLn "Connected"
  sendMessage socket =<< mkHello

  runResourceT $ do
    cxt <- openDBs False

    _ <- liftIO $ runStateT (doit socket) cxt

    return ()

