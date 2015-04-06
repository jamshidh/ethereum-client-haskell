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
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
--import Blockchain.Data.SignedTransaction
--import Blockchain.Data.Transaction
import Blockchain.Data.Wire
import Blockchain.Database.MerklePatricia
import Blockchain.DB.CodeDB
import Blockchain.DB.ModifyStateDB
import Blockchain.DBM
import Blockchain.Display
import Blockchain.PeerUrls
--import Blockchain.SampleTransactions
import Blockchain.SHA
--import Blockchain.SigningTools
import Blockchain.Util

--import Debug.Trace













import Control.Monad.IO.Class
import Crypto.PubKey.ECC.DH
import Crypto.Types.PubKey.ECC
import Crypto.Random
import qualified Data.ByteString as B
--import qualified Data.ByteString.Base16 as B16
--import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import qualified Network.Haskoin.Internals as H
--import Numeric

import Blockchain.Format
import Blockchain.Data.RLP
import Blockchain.Data.Wire
import Blockchain.SHA (SHA(..))

import Blockchain.Frame
import Blockchain.UDP
import Blockchain.RLPx
















prvKey::PrvKey
Just prvKey = makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380
--Just prvKey = makePrvKey 0xd69bceff85f3bc2d0a13bcc43b7caf6bd54a21ad0c1997ae623739216710ca19 --cpp client prvKey
--6ccf6b5c33ae2017a6c76b8791ca61276a69ab8e --cpp coinbase



getNextBlock::Block->UTCTime->ContextM Block
getNextBlock b ts = do
  let theCoinbase = prvKey2Address prvKey
  lift $ setStateRoot $ blockDataStateRoot bd
  addToBalance theCoinbase (1500*finney)

  dbs <- lift get

  return Block{
               blockBlockData=testGetNextBlockData $ stateRoot $ stateDB dbs,
               blockReceiptTransactions=[],
               blockBlockUncles=[]
             }
  where
    testGetNextBlockData::SHAPtr->BlockData
    testGetNextBlockData sr =
      BlockData {
        blockDataParentHash=blockHash b,
        blockDataUnclesHash=hash $ B.pack [0xc0],
        blockDataCoinbase=prvKey2Address prvKey,
        blockDataStateRoot = sr,
        blockDataTransactionsRoot = emptyTriePtr,
        blockDataReceiptsRoot = emptyTriePtr,
        blockDataLogBloom = B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],         
        blockDataDifficulty = nextDifficulty (blockDataDifficulty bd) (blockDataTimestamp bd) ts,
        blockDataNumber = blockDataNumber bd + 1,
        blockDataGasLimit = max 125000 ((blockDataGasLimit bd * 1023 + blockDataGasUsed bd *6 `quot` 5) `quot` 1024),
        blockDataGasUsed = 0,
        blockDataTimestamp = ts,  
        blockDataExtraData = 0,
        blockDataNonce = SHA 5
        }
    bd = blockBlockData b


submitNextBlock::Integer->Block->EthCryptM ContextM ()
submitNextBlock baseDifficulty b = do
        ts <- liftIO getCurrentTime
        newBlock <- lift $ getNextBlock b ts
        n <- liftIO $ fastFindNonce newBlock

        --let theBytes = headerHashWithoutNonce newBlock `B.append` B.pack (integer2Bytes n)
        let theNewBlock = addNonceToBlock newBlock n
        sendMsg $ NewBlockPacket theNewBlock (baseDifficulty + blockDataDifficulty (blockBlockData theNewBlock))
        lift $ addBlocks [theNewBlock]

ifBlockInDBSubmitNextBlock::Integer->Block->EthCryptM ContextM ()
ifBlockInDBSubmitNextBlock baseDifficulty b = do
  maybeBlock <- lift $ lift $ getBlock (blockHash b)
  case maybeBlock of
    Nothing -> return ()
    _ -> submitNextBlock baseDifficulty b



handleMsg::Message->EthCryptM ContextM ()
handleMsg m = do
  lift $ displayMessage False m
  case m of
    Hello{} -> do
             bestBlock <- lift getBestBlock
             genesisBlockHash <- lift getGenesisBlockHash
             sendMsg Status{
               protocolVersion=fromIntegral ethVersion,
               networkID="",
               totalDifficulty=0,
               latestHash=blockHash bestBlock,
               genesisHash=genesisBlockHash
               }
    Ping -> do
      lift addPingCount
      sendMsg Pong
    GetPeers -> do
      sendMsg $ Peers []
      sendMsg GetPeers
    (Peers thePeers) -> do
      lift $ setPeers thePeers
    BlockHashes blockHashes -> handleNewBlockHashes blockHashes
    Blocks blocks -> do
      handleNewBlocks blocks
    NewBlockPacket block baseDifficulty -> do
      lift $ addBlocks [block]
      ifBlockInDBSubmitNextBlock baseDifficulty block

    Status{latestHash=lh, genesisHash=gh} -> do
      genesisBlockHash <- lift getGenesisBlockHash
      when (gh /= genesisBlockHash) $ error "Wrong genesis block hash!!!!!!!!"
      handleNewBlockHashes [lh]
    GetTransactions -> do
      sendMsg $ Transactions []
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

readAndOutput::EthCryptM ContextM ()
readAndOutput = do
  msg <- recvMsg
  handleMsg msg
  readAndOutput
  
{-
handleAllPayloads $ getPayloads $ BL.unpack payloads
  where
    handleAllPayloads [] = error "Server has closed the connection"
    handleAllPayloads (pl:rest) = do
      handlePayload $ B.pack pl
      handleAllPayloads rest
-}

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

{-
createTransaction::Transaction->ContextM SignedTransaction
createTransaction t = do
    userNonce <- lift $ addressStateNonce <$> getAddressState (prvKey2Address prvKey)
    liftIO $ withSource devURandom $ signTransaction prvKey t{tNonce=userNonce}

createTransactions::[Transaction]->ContextM [SignedTransaction]
createTransactions transactions = do
    userNonce <- lift $ addressStateNonce <$> getAddressState (prvKey2Address prvKey)
    forM (zip transactions [userNonce..]) $ \(t, n) -> do
      liftIO $ withSource devURandom $ signTransaction prvKey t{tNonce=n}
-}

doit2::EthCryptM ContextM ()
doit2 = do
    liftIO $ putStrLn "Connected"

    lift $ lift $ addCode B.empty --This is probably a bad place to do this, but I can't think of a more natural place to do it....  Empty code is used all over the place, and it needs to be in the database.
    sendMsg =<< (liftIO mkHello)
    lift (lift . setStateRoot . blockDataStateRoot . blockBlockData =<< getBestBlock)

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


    readAndOutput

doit::String->PortNumber->ContextM ()
doit ipAddress thePort = do
  entropyPool <- liftIO createEntropyPool

  let g = cprgCreate entropyPool :: SystemRNG
      (myPriv, _) = generatePrivate g $ getCurveByName SEC_p256k1

  liftIO $ putStrLn $ "Attempting to connect to " ++ show ipAddress ++ ":" ++ show thePort
  
  otherPubKey <- liftIO $ fmap hPubKeyToPubKey $ getServerPubKey ipAddress thePort

  runEthCryptM myPriv otherPubKey ipAddress (fromIntegral thePort) doit2



--I need to use two definitions of PubKey (internally they represent the same thing)
--The one in the Haskoin package allows me to recover signatures.
--The one in the crypto packages let me do AES encryption.
--At some point I have to convert from one PubKey to the other, this function
--lets me to that.
hPubKeyToPubKey::H.PubKey->Point
hPubKeyToPubKey (H.PubKey hPoint) =
  Point (fromIntegral x) (fromIntegral y)
  where
    x = fromMaybe (error "getX failed in prvKey2Address") $ H.getX hPoint
    y = fromMaybe (error "getY failed in prvKey2Address") $ H.getY hPoint
hPubKeyToPubKey (H.PubKeyU _) = error "PubKeyU not supported in hPubKeyToPUbKey yet"

main::IO ()    
main = do
  args <- getArgs

  let ipNum =
        case args of
          (arg:_) -> arg
          [] -> "1" --Just default to poc-8.ethdev.com

  let (ipAddress, thePort) = ipAddresses !! read ipNum

  putStrLn $ "Attempting to connect to " ++ show ipAddress ++ ":" ++ show thePort
  putStrLn $ "Attempting to connect to " ++ show ipAddress ++ ":" ++ show ipNum

  runResourceT $ do
      cxt <- openDBs "h"
      _ <- runStateT (runStateT (doit ipAddress thePort) (Context [] 0 [] False)) cxt
      return ()

{-
    sendMsg Hello {
      version=3,
      clientId="dummyClient",
      capability=[ETH 60],
      port=30303,
      nodeId=0x1
      }
    liftIO . putStrLn . format =<< recvMsg
    sendMsg Ping
    liftIO . putStrLn . format =<< recvMsg
    sendMsg Pong
    liftIO . putStrLn . format =<< recvMsg
    sendMsg Status{
      protocolVersion=60,
      networkID="",
      totalDifficulty=131072,
      latestHash=SHA 0,
      genesisHash=SHA 0xfd4af92a79c7fc2fd8bf0d342f2e832e1d4f485c85b9152d2039e03bc604fdca
      }
    liftIO . putStrLn . format =<< recvMsg
    liftIO . putStrLn . format =<< recvMsg
    liftIO . putStrLn . format =<< recvMsg


  return ()

-}
