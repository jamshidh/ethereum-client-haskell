{-# LANGUAGE OverloadedStrings #-}

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
import Network.Haskoin.Crypto hiding (Address)
import Numeric
import System.Entropy
import System.Environment

import Blockchain.Frame
import Blockchain.UDP
import Blockchain.RLPx

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

prvKey::PrvKey
Just prvKey = makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380

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
        blockDataMixHash = SHA 0,
        blockDataNonce = 5
        }
    bd = blockBlockData b


submitNextBlock::Integer->Block->EthCryptM ContextM ()
submitNextBlock baseDifficulty b = do
        ts <- liftIO getCurrentTime
        newBlock <- lift $ getNextBlock b ts
        --n <- liftIO $ fastFindNonce newBlock
        n <- liftIO $ return $ fastFindNonce newBlock

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
    GetTransactions -> do
      sendMsg $ Transactions []
      --liftIO $ sendMessage handle GetTransactions
      return ()
      
    _-> return ()

readAndOutput::EthCryptM ContextM ()
readAndOutput = do
  msg <- recvMsg
  handleMsg msg
  readAndOutput
  
mkHello::Point->IO Message
mkHello peerId = do
  --let peerId = B.replicate 64 0xFF -- getEntropy 64
  return Hello {
               version = 3,
               clientId = "Ethereum(G)/v0.6.4//linux/Haskell",
               capability = [ETH ethVersion], -- , SHH shhVersion],
               port = 30303,
               nodeId = peerId
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


main::IO ()    
main = do
  args <- getArgs

  let serverNum =
        case args of
          (arg:_) -> arg
          [] -> "1" --Just default to poc-8.ethdev.com

  let (ipAddress, thePort) = ipAddresses !! read serverNum

  entropyPool <- liftIO createEntropyPool

  let g = cprgCreate entropyPool :: SystemRNG
      (myPriv, _) = generatePrivate g $ getCurveByName SEC_p256k1

  liftIO $ putStrLn $ "Attempting to connect to " ++ show ipAddress ++ ":" ++ show thePort
  
  otherPubKey@(Point x y) <- liftIO $ getServerPubKey ipAddress thePort

  putStrLn $ "server public key is : " ++ showHex x "" ++ showHex y ""

  runResourceT $ do
      cxt <- openDBs "h"
      _ <- flip runStateT cxt $
           flip runStateT (Context [] 0 [] False) $
           runEthCryptM myPriv otherPubKey ipAddress (fromIntegral thePort) $ do
             let myPublic = calculatePublic theCurve myPriv
             sendMsg =<< liftIO (mkHello myPublic)
             doit
      return ()

