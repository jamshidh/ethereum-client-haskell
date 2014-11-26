
module Data.Wire (
  Message(..),
  Capability(..),
  obj2WireMessage,
  wireMessage2Obj
  ) where

import Data.Bits
import Data.Functor
import Data.List
import Data.Word
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

--import Debug.Trace

data Capability =
  ProvidesPeerDiscoveryService | 
  ProvidesTransactionRelayingService | 
  ProvidesBlockChainQueryingService deriving (Show)

capValue::Capability->Word8
capValue ProvidesPeerDiscoveryService = 0x01
capValue ProvidesTransactionRelayingService = 0x02 
capValue ProvidesBlockChainQueryingService = 0x04

data Message =
  Hello { version::Int, clientId::String, capability::[Capability], port::Int, nodeId::Word512 } |
  Ping |
  Pong |
  GetPeers |
  Peers [Peer] |
  Transactions [SignedTransaction] | 
  Blocks [Block] |
  GetChain { parentSHAs::[SHA], numChildItems::Integer } |
  NotInChain [SHA] |
  GetTransactions deriving (Show)

instance Format Message where
  format Hello{version=ver, clientId=c, capability=cap, port=p, nodeId=n} =
    CL.blue "Hello" ++
      "    version: " ++ show ver ++ "\n" ++
      "    cliendId: " ++ show c ++ "\n" ++
      "    capability: " ++ intercalate "\n            "  (show <$> cap) ++ "\n" ++
      "    port: " ++ show p ++ "\n" ++
      "    nodeId: " ++ take 20 (padZeros 64 (showHex n "")) ++ "...."
  format Ping = CL.blue "Ping"
  format Pong = CL.blue "Pong"
  format GetPeers = CL.blue "GetPeers"
  format (Peers peers) = CL.blue "Peers: " ++ intercalate ", " (format <$> peers)
  format (Transactions transactions) =
    CL.blue "Transactions:\n    " ++ tab (intercalate "\n    " (format <$> transactions))
  format (Blocks blocks) = CL.blue "Blocks:" ++ tab("\n" ++ intercalate "\n    " (format <$> blocks))
  format (GetChain pSHAs numChild) =
    CL.blue "GetChain" ++ " (max: " ++ show numChild ++ "):\n    " ++
    intercalate ",\n    " (show . pretty <$> pSHAs)
  format (NotInChain shas) =
    CL.blue "NotInChain:" ++ 
    tab ("\n" ++ CL.red "------------------------------------------------------------------" ++
         "\n" ++ CL.red "|" ++ intercalate ",\n    " (show . pretty <$> shas) ++ CL.red "|" ++
         "\n" ++ CL.red "------------------------------------------------------------------")
  format GetTransactions = CL.blue "GetTransactions"


obj2WireMessage::RLPObject->Message
obj2WireMessage (RLPArray (RLPString "":ver:RLPString []:cId:RLPScalar cap:p:nId:[])) =
  Hello (fromInteger $ rlpDecode ver) (rlpDecode cId) capList (fromInteger $ rlpDecode p) $ rlp2Word512 nId
  where
    capList = 
      (if cap .&. 1 /= 0 then [ProvidesPeerDiscoveryService] else []) ++
      (if cap .&. 2 /= 0 then [ProvidesTransactionRelayingService] else []) ++
      (if cap .&. 3 /= 0 then [ProvidesBlockChainQueryingService] else [])
obj2WireMessage (RLPArray [RLPScalar 0x02]) = Ping
obj2WireMessage (RLPArray [RLPScalar 0x03]) = Pong
obj2WireMessage (RLPArray [RLPScalar 0x10]) = GetPeers
obj2WireMessage (RLPArray (RLPScalar 0x11:peers)) = Peers $ rlpDecode <$> peers
obj2WireMessage (RLPArray (RLPScalar 0x12:transactions)) =
  Transactions $ rlpDecode <$> transactions
obj2WireMessage (RLPArray (RLPScalar 0x13:blocks)) =
  Blocks $ rlpDecode <$> blocks

obj2WireMessage (RLPArray (RLPScalar 0x14:items)) =
  GetChain (rlpDecode <$> init items) $ rlpDecode $ last items
obj2WireMessage (RLPArray (RLPScalar 0x15:items)) =
  NotInChain $ rlpDecode <$> items
obj2WireMessage (RLPArray [RLPScalar 0x16]) = GetTransactions

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
    RLPString [],
    rlpEncode cId,
    rlpEncode $ toInteger $ foldl (.|.) 0x00 $ capValue <$> cap,
    rlpEncode $ toInteger p,
    word5122RLP nId
    ]
wireMessage2Obj Ping = RLPArray $ [RLPScalar 0x2]
wireMessage2Obj Pong = RLPArray $ [RLPScalar 0x3]
wireMessage2Obj GetPeers = RLPArray $ [RLPScalar 0x10]
wireMessage2Obj (Peers peers) = RLPArray $ (RLPScalar 0x11:(rlpEncode <$> peers))
wireMessage2Obj (Transactions transactions) =
  RLPArray (RLPScalar 0x12:(rlpEncode <$> transactions))
wireMessage2Obj (Blocks blocks) =
  RLPArray (RLPScalar 0x13:(rlpEncode <$> blocks))
wireMessage2Obj (GetChain pSHAs numChildren) = 
  RLPArray $ [RLPScalar 0x14] ++
  (rlpEncode <$> pSHAs) ++
  [rlpEncode numChildren]
wireMessage2Obj (NotInChain shas) = 
  RLPArray $ [RLPScalar 0x15] ++
  (rlpEncode <$> shas)
wireMessage2Obj GetTransactions = RLPArray [RLPScalar 0x16]

    
