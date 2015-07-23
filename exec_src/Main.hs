{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main (
  main
  ) where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import Crypto.PubKey.ECC.DH
import Crypto.Types.PubKey.ECC
import Crypto.Random
import qualified Data.ByteString as B
import Data.Time.Clock
import HFlags
import qualified Network.Haskoin.Internals as H
import Numeric
import System.Entropy
import System.Environment
import System.IO.MMap

import Blockchain.Frame
import Blockchain.UDP hiding (Ping,Pong)
import Blockchain.RLPx

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
import Blockchain.Data.Transaction
import Blockchain.Data.Wire
import Blockchain.Database.MerklePatricia
import Blockchain.DB.CodeDB
import Blockchain.DB.ModifyStateDB
import Blockchain.DBM
import Blockchain.Display
import Blockchain.PeerUrls
import Blockchain.Options
--import Blockchain.SampleTransactions
import Blockchain.SHA
--import Blockchain.SigningTools
import Blockchain.Util
import qualified Data.ByteString.Base16 as B16
--import Debug.Trace

import Data.Word
import Data.Bits
import Data.Maybe
import Cache

coinbasePrvKey::H.PrvKey
Just coinbasePrvKey = H.makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380

getNextBlock::Block->UTCTime->[Transaction]->ContextM Block
getNextBlock b ts transactions = do
  let theCoinbase = prvKey2Address coinbasePrvKey
  lift $ setStateRoot $ blockDataStateRoot bd
  addToBalance theCoinbase (1500*finney)

  dbs <- lift get

  return Block{
               blockBlockData=testGetNextBlockData $ SHAPtr "", -- $ stateRoot $ stateDB dbs,
               blockReceiptTransactions=transactions,
               blockBlockUncles=[]
             }
  where
    testGetNextBlockData::SHAPtr->BlockData
    testGetNextBlockData sr =
      BlockData {
        blockDataParentHash=blockHash b,
        blockDataUnclesHash=hash $ B.pack [0xc0],
        blockDataCoinbase=prvKey2Address coinbasePrvKey,
        blockDataStateRoot = sr,
        blockDataTransactionsRoot = emptyTriePtr,
        blockDataReceiptsRoot = emptyTriePtr,
        blockDataLogBloom = B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],         
        blockDataDifficulty = nextDifficulty (blockDataDifficulty bd) (blockDataTimestamp bd) ts,
        blockDataNumber = blockDataNumber bd + 1,
        blockDataGasLimit = blockDataGasLimit bd, -- max 125000 ((blockDataGasLimit bd * 1023 + blockDataGasUsed bd *6 `quot` 5) `quot` 1024),
        blockDataGasUsed = 0,
        blockDataTimestamp = ts,  
        blockDataExtraData = 0,
        blockDataMixHash = SHA 0,
        blockDataNonce = 5
        }
    bd = blockBlockData b


submitNextBlock::Integer->Block->EthCryptM ContextM ()
submitNextBlock baseDifficulty b = do
        ts <- liftIO getCurrentTime
        newBlock <- lift $ getNextBlock b ts []
        n <- liftIO $ fastFindNonce newBlock

        --let theBytes = headerHashWithoutNonce newBlock `B.append` B.pack (integer2Bytes n)
        let theNewBlock = addNonceToBlock newBlock n
        sendMsg $ NewBlockPacket theNewBlock (baseDifficulty + blockDataDifficulty (blockBlockData theNewBlock))
        lift $ addBlocks False [theNewBlock]

submitNextBlockToDB::Integer->Block->[Transaction]->EthCryptM ContextM ()
submitNextBlockToDB baseDifficulty b transactions = do
  ts <- liftIO getCurrentTime
  newBlock <- lift $ getNextBlock b ts transactions
  --n <- liftIO $ fastFindNonce newBlock

  let theNewBlock = addNonceToBlock newBlock (-1)
  lift $ addBlocks True [theNewBlock]

submitNewBlock::Block->[Transaction]->EthCryptM ContextM ()
submitNewBlock b transactions = do
  --lift $ addTransactions b (blockDataGasLimit $ blockBlockData b) transactions
  submitNextBlockToDB 0 b transactions

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
    GetBlocks blocks -> do
      sendMsg $ Blocks []
    Blocks blocks -> do
      handleNewBlocks blocks
    NewBlockPacket block baseDifficulty -> do
      handleNewBlocks [block]
      --ifBlockInDBSubmitNextBlock baseDifficulty block

    Status{latestHash=lh, genesisHash=gh} -> do
      genesisBlockHash <- lift getGenesisBlockHash
      when (gh /= genesisBlockHash) $ error "Wrong genesis block hash!!!!!!!!"
      handleNewBlockHashes [lh]
    (GetTransactions transactions) -> do
      sendMsg $ Transactions []
      --liftIO $ sendMessage handle GetTransactions
      return ()
    (Transactions transactions) -> do
      bestBlock <-lift getBestBlock
      submitNewBlock bestBlock transactions
      
    _-> return ()

readAndOutput::EthCryptM ContextM ()
readAndOutput = do
  msg <- recvMsg
  handleMsg msg
  readAndOutput
  
mkHello::Point->IO Message
mkHello peerId = do
  --let peerId = B.replicate 64 0xFF -- getEntropy 64
  let hello = Hello {
               version = 4,
               clientId = "Ethereum(G)/v0.6.4//linux/Haskell",
               capability = [ETH ethVersion], -- , SHH shhVersion],
               port = 0,
               nodeId = peerId
             }
--  putStrLn $ show $ wireMessage2Obj hello
--  putStrLn $ show $ rlpSerialize $ snd (wireMessage2Obj hello)
  return hello
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

intToBytes::Integer->[Word8]
intToBytes x = map (fromIntegral . (x `shiftR`)) [256-8, 256-16..0]

pointToBytes::Point->[Word8]
pointToBytes (Point x y) = intToBytes x ++ intToBytes y
pointToBytes PointO = error "pointToBytes got value PointO, I don't know what to do here"

doit::EthCryptM ContextM ()
doit = do
    liftIO $ putStrLn "Connected"

    lift $ lift $ addCode B.empty --This is probably a bad place to do this, but I can't think of a more natural place to do it....  Empty code is used all over the place, and it needs to be in the database.
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

theCurve::Curve
theCurve = getCurveByName SEC_p256k1


hPubKeyToPubKey::H.PubKey->Point
hPubKeyToPubKey (H.PubKey hPoint) = Point (fromIntegral x) (fromIntegral y)
  where
    x = fromMaybe (error "getX failed in prvKey2Address") $ H.getX hPoint
    y = fromMaybe (error "getY failed in prvKey2Address") $ H.getY hPoint
hPubKeyToPubKey (H.PubKeyU _) = error "PubKeyU not supported in hPubKeyToPUbKey yet"

main::IO ()    
main = do
  args <- $initHFlags "The Ethereum Haskell Peer"

  let (ipAddress, thePort) =
        case args of
          [] -> ipAddresses !! 1 --default server
          [x] -> ipAddresses !! read x
          ["-a", address] -> (address, 30303)
          [x, prt] -> (fst (ipAddresses !! read x), fromIntegral $ read prt)
          ["-a", address, prt] -> (address, fromIntegral $ read prt)
          _ -> error "usage: ethereumH [servernum] [port]"

  putStrLn $ "Attempting to connect to " ++ show ipAddress ++ ":" ++ show thePort

  entropyPool <- liftIO createEntropyPool

  let g = cprgCreate entropyPool :: SystemRNG
      (myPriv, _) = generatePrivate g $ getCurveByName SEC_p256k1

  let myPublic = calculatePublic theCurve myPriv
--  putStrLn $ "my pubkey is: " ++ show myPublic
  putStrLn $ "my pubkey is: " ++ (show $ B16.encode $ B.pack $ pointToBytes myPublic)
  

--  putStrLn $ "my UDP pubkey is: " ++ (show $ H.derivePubKey $ prvKey)
  putStrLn $ "my NodeID is: " ++ (show $ B16.encode $ B.pack $ pointToBytes $ hPubKeyToPubKey $ H.derivePubKey $ H.PrvKey $ fromIntegral myPriv)
    
  otherPubKey@(Point x y) <- liftIO $ getServerPubKey (H.PrvKey $ fromIntegral myPriv) ipAddress thePort


--  putStrLn $ "server public key is : " ++ (show otherPubKey)
  putStrLn $ "server public key is : " ++ (show $ B16.encode $ B.pack $ pointToBytes otherPubKey)

  --cch <- mkCache 1024 "seed"

  dataset <- return "" -- mmapFileByteString "dataset0" Nothing

  runResourceT $ do
      cxt <- openDBs "h"
      _ <- flip runStateT cxt $
           flip runStateT (Context [] 0 [] dataset False [] flags_debug) $
           runEthCryptM myPriv otherPubKey ipAddress (fromIntegral thePort) $ do
              
             sendMsg =<< liftIO (mkHello myPublic)
          
             doit
      return ()

