
module Blockchain.Data.Wire (
  Message(..),
  Capability(..),
  obj2WireMessage,
  wireMessage2Obj
  ) where

import Data.Functor
import Data.List
import Network.Haskoin.Crypto
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Blockchain.Colors as CL
import Blockchain.Data.Block
import Blockchain.Data.Peer
import Blockchain.Data.RLP
import Blockchain.Data.SignedTransaction
import Blockchain.Format
import Blockchain.SHA
import Blockchain.Util

import Debug.Trace

data Capability = ETH Integer | SHH Integer deriving (Show)

name2Cap::Integer->String->Capability
name2Cap qqqq "eth" = ETH qqqq
name2Cap qqqq "shh" = SHH qqqq
name2Cap _ x = error $ "Unknown capability string: " ++ x

{-capValue::Capability->String
capValue ETH = "eth"
capValue SHH = "shh"-}

instance RLPSerializable Capability where
    rlpEncode (ETH qqqq) = RLPArray [rlpEncode "eth", rlpEncode qqqq]
    rlpEncode (SHH qqqq) = RLPArray [rlpEncode "shh", rlpEncode qqqq]

    rlpDecode (RLPArray [name, qqqq]) = name2Cap (rlpDecode qqqq) $ rlpDecode name
    rlpDecode x = error $ "wrong format given to rlpDecode for Capability: " ++ show (pretty x)

data TerminationReason =
  DisconnectRequested
  | TCPSubSystemError
  | BreachOfProtocol
  | UselessPeer
  | TooManyPeers
  | AlreadyConnected
  | IncompatibleP2PProtocolVersion
  | NullNodeIdentityReceived
  | ClientQuitting
  | UnexpectedIdentity
  | ConnectedToSelf
  | PingTimeout
  | OtherSubprotocolReason deriving (Show)


numberToTerminationReason::Integer->TerminationReason
numberToTerminationReason 0x00 = DisconnectRequested
numberToTerminationReason 0x01 = TCPSubSystemError
numberToTerminationReason 0x02 = BreachOfProtocol
numberToTerminationReason 0x03 = UselessPeer
numberToTerminationReason 0x04 = TooManyPeers
numberToTerminationReason 0x05 = AlreadyConnected
numberToTerminationReason 0x06 = IncompatibleP2PProtocolVersion
numberToTerminationReason 0x07 = NullNodeIdentityReceived
numberToTerminationReason 0x08 = ClientQuitting
numberToTerminationReason 0x09 = UnexpectedIdentity
numberToTerminationReason 0x0a = ConnectedToSelf
numberToTerminationReason 0x0b = PingTimeout
numberToTerminationReason 0x0c = OtherSubprotocolReason
numberToTerminationReason _ = error "numberToTerminationReasion called with unsupported number"


terminationReasonToNumber::TerminationReason->Integer
terminationReasonToNumber DisconnectRequested = 0x00
terminationReasonToNumber TCPSubSystemError = 0x01
terminationReasonToNumber BreachOfProtocol = 0x02
terminationReasonToNumber UselessPeer = 0x03
terminationReasonToNumber TooManyPeers = 0x04
terminationReasonToNumber AlreadyConnected = 0x05
terminationReasonToNumber IncompatibleP2PProtocolVersion = 0x06
terminationReasonToNumber NullNodeIdentityReceived = 0x07
terminationReasonToNumber ClientQuitting = 0x08
terminationReasonToNumber UnexpectedIdentity = 0x09
terminationReasonToNumber ConnectedToSelf = 0x0a
terminationReasonToNumber PingTimeout = 0x0b
terminationReasonToNumber OtherSubprotocolReason = 0x0c
  


data Message =
  Hello { version::Int, clientId::String, capability::[Capability], port::Int, nodeId::Word512 } |
  Disconnect TerminationReason |
  Ping |
  Pong |
  GetPeers |
  Peers [Peer] |
  Status { protocolVersion::Int, networkID::String, totalDifficulty::Int, latestHash::SHA, genesisHash:: SHA } |
  QqqqStatus Int |
  Transactions [SignedTransaction] | 
  GetBlocks [SHA] |
  Blocks [Block] |
  BlockHashes [SHA] |
  GetBlockHashes { parentSHAs::[SHA], numChildItems::Integer } |
  GetTransactions |
  NewBlockPacket Block Integer |
  PacketCount Integer |
  QqqqPacket |
  WhisperProtocolVersion Int deriving (Show)

instance Format Message where
  format Hello{version=ver, clientId=c, capability=cap, port=p, nodeId=n} =
    CL.blue "Hello" ++
      "    version: " ++ show ver ++ "\n" ++
      "    cliendId: " ++ show c ++ "\n" ++
      "    capability: " ++ intercalate ", " (show <$> cap) ++ "\n" ++
      "    port: " ++ show p ++ "\n" ++
      "    nodeId: " ++ take 20 (padZeros 64 (showHex n "")) ++ "...."
  format (Disconnect reason) = CL.blue "Disconnect" ++ "(" ++ show reason ++ ")"
  format Ping = CL.blue "Ping"
  format Pong = CL.blue "Pong"
  format GetPeers = CL.blue "GetPeers"
  format (Peers peers) = CL.blue "Peers: " ++ intercalate ", " (format <$> peers)
  format Status{ protocolVersion=ver, networkID=nID, totalDifficulty=d, latestHash=lh, genesisHash=gh } =
    CL.blue "Status" ++
      "    protocolVersion: " ++ show ver ++ "\n" ++
      "    networkID: " ++ show nID ++ "\n" ++
      "    totalDifficulty: " ++ show d ++ "\n" ++
      "    latestHash: " ++ show (pretty lh) ++ "\n" ++
      "    genesisHash: " ++ show (pretty gh)
  format (QqqqStatus ver) =
    CL.blue "QqqqStatus " ++
      "    protocolVersion: " ++ show ver
  format (Transactions transactions) =
    CL.blue "Transactions:\n    " ++ tab (intercalate "\n    " (format <$> transactions))
    
--Short version
  format (BlockHashes shas) =
    CL.blue "BlockHashes " ++ "(" ++ show (length shas) ++ " new hashes)" 
--Long version
{-  format (BlockHashes shas) =
    CL.blue "BlockHashes:" ++  
   tab ("\n" ++ intercalate "\n    " (show . pretty <$> shas))-}

  format (GetBlocks shas) =
    CL.blue "GetBlocks:" ++ 
    tab ("\n" ++ intercalate "\n    " (show . pretty <$> shas))
  format (Blocks blocks) = CL.blue "Blocks:" ++ tab("\n" ++ intercalate "\n    " (format <$> blocks))
  format (GetBlockHashes pSHAs numChild) =
    CL.blue "GetBlockHashes" ++ " (max: " ++ show numChild ++ "):\n    " ++
    intercalate ",\n    " (show . pretty <$> pSHAs)
  format (NewBlockPacket block d) = CL.blue "NewBlockPacket" ++ " (" ++ show d ++ ")" ++ tab ("\n" ++ format block)
  format (PacketCount c) =
    CL.blue "PacketCount:" ++ show c
  format QqqqPacket = CL.blue "QqqqPacket"
  format GetTransactions = CL.blue "GetTransactions"
  format (WhisperProtocolVersion ver) = CL.blue "WhisperProtocolVersion " ++ show ver


obj2WireMessage::RLPObject->Message
obj2WireMessage (RLPArray [RLPString "", ver, cId, RLPArray cap, p, nId]) =
  Hello (fromInteger $ rlpDecode ver) (rlpDecode cId) (rlpDecode <$> cap) (fromInteger $ rlpDecode p) $ rlp2Word512 nId
obj2WireMessage (RLPArray [RLPScalar 0x01, reason]) =
  Disconnect (numberToTerminationReason $ rlpDecode reason)
obj2WireMessage (RLPArray [RLPScalar 0x02]) = Ping
obj2WireMessage (RLPArray [RLPScalar 0x02, RLPArray []]) = Ping
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
obj2WireMessage (RLPArray [RLPScalar 0x10, ver]) = 
    QqqqStatus $ fromInteger $ rlpDecode ver

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
obj2WireMessage (RLPArray [RLPScalar 0x17, block, td]) =
  NewBlockPacket (rlpDecode block) (rlpDecode td)
obj2WireMessage (RLPArray [RLPScalar 0x18, c]) =
  PacketCount $ rlpDecode c
obj2WireMessage (RLPArray [RLPScalar 0x19]) =
  QqqqPacket

obj2WireMessage (RLPArray [RLPScalar 0x20, ver]) =
  WhisperProtocolVersion $ fromInteger $ rlpDecode ver

obj2WireMessage x = error ("Missing case in obj2WireMessage: " ++ show (pretty x))






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
    RLPArray $ rlpEncode <$> cap,
    rlpEncode $ toInteger p,
    word5122RLP nId
    ]
wireMessage2Obj (Disconnect reason) =
  RLPArray [RLPScalar 0x1, rlpEncode $ terminationReasonToNumber reason]
wireMessage2Obj Ping = RLPArray [RLPScalar 0x2]
wireMessage2Obj Pong = RLPArray [RLPScalar 0x3]
wireMessage2Obj GetPeers = RLPArray [RLPScalar 0x4]
wireMessage2Obj (Peers peers) = RLPArray $ RLPScalar 0x5:(rlpEncode <$> peers)

wireMessage2Obj (Status ver nID d lh gh) =
    RLPArray [RLPScalar 0x10, rlpEncode $ toInteger ver, rlpEncode nID, rlpEncode $ toInteger d, rlpEncode lh, rlpEncode gh]
wireMessage2Obj (QqqqStatus ver) =
    RLPArray [RLPScalar 0x10, rlpEncode $ toInteger ver]
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
wireMessage2Obj (NewBlockPacket block d) =
  RLPArray [RLPScalar 0x17, rlpEncode block, rlpEncode d]
wireMessage2Obj (PacketCount c) =
  RLPArray [RLPScalar 0x18, rlpEncode c]
wireMessage2Obj QqqqPacket =
  RLPArray [RLPScalar 0x19]

wireMessage2Obj (WhisperProtocolVersion ver) = 
  RLPArray [RLPScalar 0x20, rlpEncode $ toInteger ver]



