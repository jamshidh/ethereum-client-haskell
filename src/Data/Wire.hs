
module Data.Wire (
  Message(..),
  Capability(..),
  obj2WireMessage,
  wireMessage2Obj,
  sendMessage
  ) where

import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Network.Haskoin.Crypto
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Data.Block
import qualified Colors as CL
import Format
import Data.Peer
import Data.RLP
import SHA
import Data.SignedTransaction
import Util

import Network.Simple.TCP


--import Debug.Trace

data Capability = ETH | SHH deriving (Show)

capValue::Capability->String
capValue ETH = "eth"
capValue SHH = "shh"

data Message =
  Hello { version::Int, clientId::String, capability::[Capability], port::Int, nodeId::Word512 } |
  Ping |
  Pong |
  GetPeers |
  Peers [Peer] |
  Status { protocolVersion::Int, networkID::String, totalDifficulty::Int, latestHash::SHA, genesisHash:: SHA } |
  Transactions [SignedTransaction] | 
  GetBlocks [SHA] |
  Blocks [Block] |
  BlockHashes [SHA] |
  GetBlockHashes { parentSHAs::[SHA], numChildItems::Integer } |
  GetTransactions |
  WhisperProtocolVersion Int deriving (Show)

instance Format Message where
  format Hello{version=ver, clientId=c, capability=cap, port=p, nodeId=n} =
    CL.blue "Hello" ++
      "    version: " ++ show ver ++ "\n" ++
      "    cliendId: " ++ show c ++ "\n" ++
      "    capability: " ++ intercalate ", " (show <$> cap) ++ "\n" ++
      "    port: " ++ show p ++ "\n" ++
      "    nodeId: " ++ take 20 (padZeros 64 (showHex n "")) ++ "...."
  format Ping = CL.blue "Ping"
  format Pong = CL.blue "Pong"
  format GetPeers = CL.blue "GetPeers"
  format (Peers peers) = CL.blue "Peers: " ++ intercalate ", " (format <$> peers)
  format Status{ protocolVersion=ver, networkID=nID, totalDifficulty=d, latestHash=lh, genesisHash=gh } =
    CL.blue "Helloe" ++
      "    protocolVersion: " ++ show ver ++ "\n" ++
      "    networkID: " ++ show nID ++ "\n" ++
      "    totalDifficulty: " ++ show d ++ "\n" ++
      "    latestHash: " ++ show (pretty lh) ++ "\n" ++
      "    genesisHash: " ++ show (pretty gh)
  format (Transactions transactions) =
    CL.blue "Transactions:\n    " ++ tab (intercalate "\n    " (format <$> transactions))
  format (BlockHashes shas) =
    CL.blue "BlockHashes:" ++ 
    tab ("\n" ++ intercalate "\n    " (show . pretty <$> shas))
  format (GetBlocks shas) =
    CL.blue "GetBlocks:" ++ 
    tab ("\n" ++ intercalate "\n    " (show . pretty <$> shas))
  format (Blocks blocks) = CL.blue "Blocks:" ++ tab("\n" ++ intercalate "\n    " (format <$> blocks))
  format (GetBlockHashes pSHAs numChild) =
    CL.blue "GetBlockHashes" ++ " (max: " ++ show numChild ++ "):\n    " ++
    intercalate ",\n    " (show . pretty <$> pSHAs)
  format GetTransactions = CL.blue "GetTransactions"
  format (WhisperProtocolVersion ver) = CL.blue "WhisperProtocolVersion " ++ show ver


obj2WireMessage::RLPObject->Message
obj2WireMessage (RLPArray [RLPString "", ver, cId, RLPArray cap, p, nId]) =
  Hello (fromInteger $ rlpDecode ver) (rlpDecode cId) (capStr2Cap <$> rlpDecode <$> cap) (fromInteger $ rlpDecode p) $ rlp2Word512 nId
  where
    capStr2Cap "eth" = ETH
    capStr2Cap "shh" = SHH
    capStr2Cap x = error $ "Unknown capability string: " ++ x

{-obj2WireMessage (RLPArray [RLPString "", ver, RLPString [], cId, RLPScalar cap, p, nId]) =
  Hello (fromInteger $ rlpDecode ver) (rlpDecode cId) capList (fromInteger $ rlpDecode p) $ rlp2Word512 nId
  where
    capList = 
      [ProvidesPeerDiscoveryService|testBit cap 1] ++
      [ProvidesTransactionRelayingService|testBit cap 2] ++
      [ProvidesBlockChainQueryingService|testBit cap 3]-}
obj2WireMessage (RLPArray [RLPScalar 0x02]) = Ping
obj2WireMessage (RLPArray [RLPScalar 0x03]) = Pong
obj2WireMessage (RLPArray [RLPScalar 0x04]) = GetPeers
obj2WireMessage (RLPArray (RLPScalar 0x05:peers)) = Peers $ rlpDecode <$> peers
obj2WireMessage (RLPArray [RLPScalar 0x10, ver, nID, d, lh, gh]) = 
    Status {
  protocolVersion=fromInteger $ rlpDecode ver,
  networkID = rlpDecode nID,
  totalDifficulty = fromInteger $ rlpDecode d,
  latestHash=rlpDecode lh,
  genesisHash=rlpDecode gh
}

obj2WireMessage (RLPArray [RLPScalar 0x11]) = GetTransactions
obj2WireMessage (RLPArray (RLPScalar 0x12:transactions)) =
  Transactions $ rlpDecode <$> transactions


obj2WireMessage (RLPArray (RLPScalar 0x13:items)) =
  GetBlockHashes (rlpDecode <$> init items) $ rlpDecode $ last items
obj2WireMessage (RLPArray (RLPScalar 0x14:items)) =
  BlockHashes $ rlpDecode <$> items


obj2WireMessage (RLPArray (RLPScalar 0x15:items)) =
  GetBlocks $ rlpDecode <$> items
obj2WireMessage (RLPArray (RLPScalar 0x16:blocks)) =
  Blocks $ rlpDecode <$> blocks

obj2WireMessage (RLPArray [RLPScalar 0x20, ver]) =
  WhisperProtocolVersion $ fromInteger $ rlpDecode ver

obj2WireMessage x = error ("Missing case in obj2WireMessage: " ++ show x)


wireMessage2Obj::Message->RLPObject
wireMessage2Obj Hello { version = ver,
                        clientId = cId,
                        capability = cap,
                        port = p,
                        nodeId = nId } =
  RLPArray [
    RLPString [],
    rlpEncode $ toInteger ver,
    rlpEncode cId,
    RLPArray $ rlpEncode <$> capValue <$> cap,
    rlpEncode $ toInteger p,
    word5122RLP nId
    ]
wireMessage2Obj Ping = RLPArray [RLPScalar 0x2]
wireMessage2Obj Pong = RLPArray [RLPScalar 0x3]
wireMessage2Obj GetPeers = RLPArray [RLPScalar 0x4]
wireMessage2Obj (Peers peers) = RLPArray $ RLPScalar 0x5:(rlpEncode <$> peers)

wireMessage2Obj (Status ver nID d lh gh) =
    RLPArray [RLPScalar 0x10, rlpEncode $ toInteger ver, rlpEncode nID, rlpEncode $ toInteger d, rlpEncode lh, rlpEncode gh]

wireMessage2Obj GetTransactions = RLPArray [RLPScalar 0x11]
wireMessage2Obj (Transactions transactions) =
  RLPArray (RLPScalar 0x12:(rlpEncode <$> transactions))


wireMessage2Obj (GetBlockHashes pSHAs numChildren) = 
  RLPArray $ [RLPScalar 0x13] ++
  (rlpEncode <$> pSHAs) ++
  [rlpEncode numChildren]
wireMessage2Obj (BlockHashes shas) = 
  RLPArray $ RLPScalar 0x14:(rlpEncode <$> shas)


wireMessage2Obj (GetBlocks shas) = 
  RLPArray $ RLPScalar 0x15:(rlpEncode <$> shas)
wireMessage2Obj (Blocks blocks) =
  RLPArray (RLPScalar 0x16:(rlpEncode <$> blocks))

wireMessage2Obj (WhisperProtocolVersion ver) = 
  RLPArray [RLPScalar 0x20, rlpEncode $ toInteger ver]

ethereumHeader::B.ByteString->Put
ethereumHeader payload = do
  putWord32be 0x22400891
  putWord32be $ fromIntegral $ B.length payload
  putByteString payload

    
sendCommand::Socket->B.ByteString->IO ()
sendCommand socket payload = do
  let theData2 = runPut $ ethereumHeader payload
  send socket $ B.concat $ BL.toChunks theData2

sendMessage::Socket->Message->IO ()
sendMessage socket msg = do
  putStrLn (CL.green "msg>>>>>: " ++ format msg)
  sendCommand socket $ rlpSerialize $ wireMessage2Obj msg

