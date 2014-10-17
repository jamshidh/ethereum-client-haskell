{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (
  main
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Crypto.Hash.SHA3 as C
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal
import qualified Data.ByteString.Base16 as B16
import Data.Default
import Data.Functor
import Data.Function
import Data.List
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import qualified Database.LevelDB as DB
import Network.Haskoin.Crypto hiding (Address)
import Network.Haskoin.Internals hiding (Ping, Pong, version, Message, Block, timestamp, Address)
import Network.Socket (socketToHandle)
import Numeric
import System.Directory
import System.IO

--remove this
--import Data.Time

import Network.Simple.TCP

import Address
import AddressState
import Block
import BlockChain
import Colors
import Constants
import EthDB
import Format
import ModifyStateDB
import RLP
import SHA
--import Transaction
import Util
import Wire

--import Debug.Trace

{-
data IPAddress = IPV4Address Word8 Word8 Word8 Word8 |
  IPV6Address Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 
-}


prvKey::PrvKey
Just prvKey = makePrvKey 0xac3e8ce2ef31c3f45d5da860bcd9aee4b37a05c5a3ddee40dd061620c3dab380
--Just prvKey = makePrvKey 0xd69bceff85f3bc2d0a13bcc43b7caf6bd54a21ad0c1997ae623739216710ca19 --cpp client prvKey
--6ccf6b5c33ae2017a6c76b8791ca61276a69ab8e --cpp coinbase


{-
address::IPAddress->Word16->Word64->Put
address (IPV4Address d1 d2 d3 d4) port flags = do
  putWord64le flags
  putByteString $ B.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF]
  putWord8 d1
  putWord8 d2
  putWord8 d3
  putWord8 d4
  putWord16le port
-}

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

{-
addReward::SHAPtr->Address->IO SHAPtr
addReward stateRoot address = 
  runResourceT $ do
    liftIO $ putStrLn "about to open"
    homeDir <- liftIO $ getHomeDirectory
    db <- DB.open (homeDir ++ "/" ++ stateDBPath) DB.defaultOptions{DB.createIfMissing=True}
    liftIO $ putStrLn "opened"
    DB.put db def startingRoot B.empty

    addressState <- getAddressState db stateRoot address

    putAddressState db stateRoot address (addressState{balance=balance addressState + fromIntegral (1500*finney)})
-}
  
getNextBlock::Block->UTCTime->IO Block
getNextBlock b ts = do
  let theCoinbase = prvKey2Address prvKey
  newStateRoot <- addReward (stateRoot bd) theCoinbase

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
        sendMessage socket $ Blocks [addNonceToBlock newBlock n]
              



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
        _ -> return ()
      
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

main1::IO ()    
main1 = connect "127.0.0.1" "30303" $ \(socket, _) -> do
--main1 = connect "192.168.0.2" "30303" $ \(socket, _) -> do
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

  {-
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
  -}
  
  let b = Block{blockData=
                   BlockData {
                     parentHash=SHA 0,
                     unclesHash=hash $ B.pack [0xc0],
                     coinbase=prvKey2Address prvKey,
                     stateRoot = SHAPtr $ B.pack $ integer2Bytes 1,
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
  --let ts = 0

  ts <- getCurrentTime
  
  --let newBlock = testGetNextBlock genesisBlock ts
  --let powVal = byteString2Integer $ BC.pack $ powFunc newBlock
  --putStrLn $ "powFunc = " ++ show (showHex powVal "")
  --let passed = powVal * (difficulty $ blockData newBlock) < 2^256
  --putStrLn (red "Passed: " ++ show passed)
  --theNonce <- (fastFindNonce newBlock)::IO Integer
  --sendMessage socket $ Blocks [addNonceToBlock newBlock theNonce]


  --sendMessage socket $ Transactions [signedTx]
  maybeBestBlockHash <- withBlockDB getBestBlockHash
  bestBlockHash <-
    case maybeBestBlockHash of
      Nothing -> do
        initializeBlockChain
        initializeStateDB
        return $ blockHash genesisBlock
      Just x -> return x
  putStrLn $ "Best block hash: " ++ format bestBlockHash
  sendMessage socket $ GetChain [bestBlockHash] 0x40
  --sendMessage socket $ GetChain [blockHash genesisBlock] 0x40
  putStrLn "Transaction has been sent"

  readAndOutput socket

main2::IO ()    
main2 = do
  let b = 
        Block {
          blockData = BlockData {
             parentHash = SHA (BigWord {getBigWordInteger = 877290184733011228355131318509245476995638757136170869201259140273173077028}),
             unclesHash = SHA (BigWord {getBigWordInteger = 13478047122767188135818125966132228187941283477090363246179690878162135454535}),
             coinbase = Address (BigWord {getBigWordInteger = 521006474229988785287787995209817519331962723518}),
             stateRoot = SHAPtr $ B.pack $ integer2Bytes 1, transactionsTrie = 0,
             difficulty = 12917270,
             number = 0,
             minGasPrice = 10000000000000,
             gasLimit = 125000,
             gasUsed = 0,
             timestamp = read "2014-09-26 20:27:03 UTC",
             extraData = 0,
             nonce = SHA (BigWord {getBigWordInteger = 5})},
          receiptTransactions = [],
          blockUncles = []
          }
  print b
  n <- fastFindNonce b
  print $ showHex n ""
  let final = headerHashWithoutNonce b `B.append` B.pack (integer2Bytes n)
  print $ format final
  print $ format $ C.hash 256 final

-------------------------

  

--"/home/jim/.ethereum/state/"
--"/Users/hutong/Library/Application Support/Ethereum/state/"
--"/tmp/leveldbtest"

startingRoot::B.ByteString
(startingRoot, "") = B16.decode "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
                     --"bc36789e7a1e281436464229828f817d6612f7b477d66591ff96a9e064bcc98a"
                    

genesisRoot::B.ByteString
(genesisRoot, "") = B16.decode "8dbd704eb38d1c2b73ee4788715ea5828a030650829703f077729b2b613dd206"

stateRoot1::B.ByteString
(stateRoot1, "") = B16.decode "8b9a53acdafe82b6247c195035995f892f467fda03d7b44b684dcb3663fb3497"

stateRoot2::B.ByteString
(stateRoot2, "") = B16.decode "b995f323e0811206ec8ed8cadf334a713413eca003c8cbde75a777a647eb6ec7"

stateRoot3::B.ByteString
(stateRoot3, "") = B16.decode "f22a63cd45e9560b36f1f5cfb95f917601493650d1f5cc237d3f4893ab45f3b9"

stateRoot4::B.ByteString
(stateRoot4, "") = B16.decode "5893c00aa5e6fcb867d916207fa8cf7339689b9f85aafa92b81574c6e438f887"


showStuff::DB.DB->ResourceT IO ()
showStuff db = do
    i <- DB.iterOpen db def
    DB.iterFirst i
    sequence_ $ replicate 5 $ DB.iterNext i
    Just key <- DB.iterKey i

    theData1 <- getKeyVals db (SHAPtr key) ""
    liftIO $ putStrLn $ intercalate "\n" $ kvFormat <$> theData1

    liftIO $ putStrLn "========================"
    Just genesisData <- getAddressState db (SHAPtr genesisRoot) $ Address 0x51ba59315b3a95761d0863b05ccc7a7f54703d99
    liftIO $ putStrLn $ format $ genesisData

    liftIO $ putStrLn "========================"
    Just b1Data <- getAddressState db (SHAPtr stateRoot1) $ Address 0x6ccf6b5c33ae2017a6c76b8791ca61276a69ab8e
    liftIO $ putStrLn $ format $ b1Data

    liftIO $ putStrLn "========================"
    Just b2Data <- getAddressState db (SHAPtr stateRoot2) $ Address 0x6ccf6b5c33ae2017a6c76b8791ca61276a69ab8e
    liftIO $ putStrLn $ format $ b2Data

    liftIO $ putStrLn "========================"
    Just b3Data <- getAddressState db (SHAPtr stateRoot3) $ Address 0x6ccf6b5c33ae2017a6c76b8791ca61276a69ab8e
    liftIO $ putStrLn $ format $ b3Data

    liftIO $ putStrLn "========================"
    Just b4Data <- getAddressState db (SHAPtr stateRoot4) $ Address 0x6ccf6b5c33ae2017a6c76b8791ca61276a69ab8e
    liftIO $ putStrLn $ format $ b4Data

startingAddressState =
      AddressState {
      addressStateNonce=0,
      balance= 0x0100000000000000000000000000000000000000000000000000,
      contractRoot=0,
      codeHash=hash B.empty
      }

main3::IO ()
main3 = do
  let options = DB.defaultOptions {
        DB.createIfMissing=True, DB.cacheSize=1024}
  runResourceT $ do
    db <- DB.open stateDBPath options
    DB.put db def startingRoot B.empty

    --showStuff db

    let addresses = Address <$> [
          0x51ba59315b3a95761d0863b05ccc7a7f54703d99,
          0xe6716f9544a56c530d868e4bfbacb172315bdead,
          0xb9c015918bdaba24b4ff057a92a3873d6eb201be,
          0x1a26338f0d905e295fccb71fa9ea849ffa12aaf4,
          0x2ef47100e0787b915105fd5e3f4ff6752079d5cb,
          0xcd2a3d9f938e13cd947ec05abc7fe734df8dd826,
          0x6c386a4b26f73c802f34673f7248bb118f97424a,
          0xe4157b34ea9615cfbde6b4fda419828124b70c78
          ]

    newStateRoot <- putAddressStates db (SHAPtr startingRoot) addresses startingAddressState

    let pubKey = prvKey2Address prvKey 

    --let pubKey = Address 0x5b42bd01ff7b368cd80a477cb1cf0d407e2b1cbe

    liftIO $ putStrLn $ "Pub Key: " ++ format pubKey
    
    stateRoot2 <- putAddressState db newStateRoot pubKey
        AddressState {
          addressStateNonce=0,
          balance= fromInteger $ 1500*finney,
          contractRoot=0,
          codeHash=hash B.empty
          }

    liftIO $ putStrLn $ "New stateRoot: " ++ format stateRoot2

    --showAllKeyVal db




    liftIO $ putStrLn "========================"
    liftIO $ putStrLn $ format newStateRoot
    let SHAPtr x = newStateRoot
    allData <- getAllAddressStates db newStateRoot
    liftIO $ putStrLn $ intercalate "\n" $ (\(a, addressState) -> format a ++ tab ("\n" ++ format addressState)) <$> allData



    return ()

putAddressStates::DB.DB->SHAPtr->[Address]->AddressState->ResourceT IO SHAPtr
putAddressStates _ stateRoot [] _ = return stateRoot
putAddressStates db stateRoot (address:rest) addressState = do
    newStateRoot <- putAddressState db stateRoot address addressState
    putAddressStates db newStateRoot rest addressState

  {-
s_ret[Address(fromHex(i))] = 
AddressState(0, u256(1) << 200, h256(), EmptySHA3);

putKeyVal "1a26338f0d905e295fccb71fa9ea849ffa12aaf4" [0x0, 0x0100000000000000000000000000000000000000000000000000, 0x0, 0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470]
-}


main::IO ()    
main = do
  --main3
  --main2
  main1
