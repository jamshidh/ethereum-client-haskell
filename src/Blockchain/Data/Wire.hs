
module Blockchain.Data.Wire (
  Message(..),
  Capability(..),
  obj2WireMessage,
  wireMessage2Obj
  ) where

import Data.Functor
import Data.List
import Data.Word
import Network.Haskoin.Crypto
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Blockchain.Colors as CL
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
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


obj2WireMessage::Word8->RLPObject->Message
obj2WireMessage 0x0 (RLPArray [ver, cId, RLPArray cap, p, nId]) =
  Hello (fromInteger $ rlpDecode ver) (rlpDecode cId) (rlpDecode <$> cap) (fromInteger $ rlpDecode p) $ rlp2Word512 nId
obj2WireMessage 0x1 (RLPArray [reason]) =
  Disconnect (numberToTerminationReason $ rlpDecode reason)
obj2WireMessage 0x2 (RLPArray []) = Ping
obj2WireMessage 0x2 (RLPArray [RLPArray []]) = Ping
obj2WireMessage 0x3 (RLPArray []) = Pong
obj2WireMessage 0x4 (RLPArray []) = GetPeers
obj2WireMessage 0x5 (RLPArray peers) = Peers $ rlpDecode <$> peers
obj2WireMessage 0x10 (RLPArray [ver, nID, d, lh, gh]) = 
    Status {
  protocolVersion=fromInteger $ rlpDecode ver,
  networkID = rlpDecode nID,
  totalDifficulty = fromInteger $ rlpDecode d,
  latestHash=rlpDecode lh,
  genesisHash=rlpDecode gh
}
obj2WireMessage 0x10 (RLPArray [ver]) = 
    QqqqStatus $ fromInteger $ rlpDecode ver

obj2WireMessage 0x11 (RLPArray []) = GetTransactions
obj2WireMessage 0x12 (RLPArray transactions) =
  Transactions $ rlpDecode <$> transactions


obj2WireMessage 0x13 (RLPArray items) =
  GetBlockHashes (rlpDecode <$> init items) $ rlpDecode $ last items
obj2WireMessage 0x14 (RLPArray items) =
  BlockHashes $ rlpDecode <$> items


obj2WireMessage 0x15 (RLPArray items) =
  GetBlocks $ rlpDecode <$> items
obj2WireMessage 0x16 (RLPArray blocks) =
  Blocks $ rlpDecode <$> blocks
obj2WireMessage 0x17 (RLPArray [block, td]) =
  NewBlockPacket (rlpDecode block) (rlpDecode td)
obj2WireMessage 0x18 (RLPArray [c]) =
  PacketCount $ rlpDecode c
obj2WireMessage 0x19 (RLPArray []) =
  QqqqPacket

obj2WireMessage 0x20 (RLPArray [ver]) =
  WhisperProtocolVersion $ fromInteger $ rlpDecode ver

obj2WireMessage x y = error ("Missing case in obj2WireMessage: " ++ show x ++ ", " ++ show (pretty y))






wireMessage2Obj::Message->(Word8, RLPObject)
wireMessage2Obj Hello { version = ver,
                        clientId = cId,
                        capability = cap,
                        port = p,
                        nodeId = nId } =
  (0x0, RLPArray [
           rlpEncode $ toInteger ver,
           rlpEncode cId,
           RLPArray $ rlpEncode <$> cap,
           rlpEncode $ toInteger p,
           word5122RLP nId
          ])
wireMessage2Obj (Disconnect reason) = (0x0, RLPArray [rlpEncode $ terminationReasonToNumber reason])
wireMessage2Obj Ping = (0x2, RLPArray [])
wireMessage2Obj Pong = (0x3, RLPArray [])
wireMessage2Obj GetPeers = (0x4, RLPArray [])
wireMessage2Obj (Peers peers) = (0x5, RLPArray $ (rlpEncode <$> peers))
wireMessage2Obj (Status ver nID d lh gh) =
    (0x10, RLPArray [rlpEncode $ toInteger ver, rlpEncode nID, rlpEncode $ toInteger d, rlpEncode lh, rlpEncode gh])
wireMessage2Obj (QqqqStatus ver) = (0x10, RLPArray [rlpEncode $ toInteger ver])
wireMessage2Obj GetTransactions = (0x11, RLPArray [])
wireMessage2Obj (Transactions transactions) = (0x12, RLPArray (rlpEncode <$> transactions))
wireMessage2Obj (GetBlockHashes pSHAs numChildren) = 
    (0x13, RLPArray $ (rlpEncode <$> pSHAs) ++ [rlpEncode numChildren])
wireMessage2Obj (BlockHashes shas) = 
  (0x14, RLPArray (rlpEncode <$> shas))
wireMessage2Obj (GetBlocks shas) = 
  (0x15, RLPArray (rlpEncode <$> shas))
wireMessage2Obj (Blocks blocks) =
  (0x16, RLPArray (rlpEncode <$> blocks))
wireMessage2Obj (NewBlockPacket block d) =
  (0x17, RLPArray [rlpEncode block, rlpEncode d])
wireMessage2Obj (PacketCount c) =
  (0x18, RLPArray [rlpEncode c])
wireMessage2Obj QqqqPacket =
  (0x19, RLPArray [])

wireMessage2Obj (WhisperProtocolVersion ver) = 
  (0x20, RLPArray [rlpEncode $ toInteger ver])



