{-# OPTIONS_GHC -Wall #-}

module Wire (
  Message(..),
  Capability(..),
  obj2WireMessage,
  wireMessage2Obj
  ) where

import Data.Bits
import Data.ByteString.Internal
import Data.Functor
import Data.List
import Data.Time.Clock.POSIX
import Data.Word
import Network.Haskoin.Crypto
import Numeric

import Address
import Block
import Colors
import Format
import Peer
import Transaction
import TransactionReceipt
import RLP
import SHA

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
  Transactions [Transaction] | 
  Blocks [Block] |
  GetChain { parentSHAs::[SHA], numChildItems::Integer } |
  NotInChain [SHA] |
  GetTransactions deriving (Show)

instance Format Message where
  format Hello{version=v, clientId=c, capability=cap, port=p, nodeId=n} =
    blue "Hello" ++
      "    version: " ++ show v ++ "\n" ++
      "    cliendId: " ++ show c ++ "\n" ++
      "    capability: " ++ intercalate "\n            "  (show <$> cap) ++ "\n" ++
      "    port: " ++ show p ++ "\n" ++
      "    nodeId: " ++ take 20 (padZeros 64 (showHex n "")) ++ "...."
  format Ping = blue "Ping"
  format Pong = blue "Pong"
  format (Peers peers) = blue "Peers: " ++ intercalate ", " (format <$> peers)
  format (Blocks blocks) = blue "Blocks:\n    " ++ intercalate "\n    " (format <$> blocks)
  format (GetChain pSHAs numChild) =
    blue "GetChain" ++ " (max: " ++ show numChild ++ "):\n    " ++
    intercalate ",\n    " (format <$> pSHAs)
  format GetTransactions = blue "GetTransactions"
  format x = show x


obj2WireMessage::RLPObject->Message
obj2WireMessage (RLPArray (RLPNumber 0x00:RLPNumber v:RLPNumber 0:RLPString cId:RLPNumber cap:RLPNumber p:nId:[])) =
  Hello v cId capList p $ rlp2Word512 nId
  where
    capList = 
      (if cap .&. 1 /= 0 then [ProvidesPeerDiscoveryService] else []) ++
      (if cap .&. 2 /= 0 then [ProvidesTransactionRelayingService] else []) ++
      (if cap .&. 3 /= 0 then [ProvidesBlockChainQueryingService] else [])
obj2WireMessage (RLPArray [RLPNumber 0x02]) = Ping
obj2WireMessage (RLPArray [RLPNumber 0x03]) = Pong
obj2WireMessage (RLPArray [RLPNumber 0x10]) = GetPeers
obj2WireMessage (RLPArray (RLPNumber 0x11:peers)) = Peers $ rlp2Peer <$> peers
obj2WireMessage (RLPArray (RLPNumber 0x12:transactions)) =
  Transactions $ rlp2Transaction <$> transactions
obj2WireMessage (RLPArray (RLPNumber 0x13:blocks)) =
  Blocks $ getBlockFromRLP <$> blocks

obj2WireMessage (RLPArray (RLPNumber 0x14:items)) =
  GetChain parentSHAs $ fromIntegral numChildren
  where
    RLPNumber numChildren = last items
    parentSHAs = rlp2SHA <$> init items

obj2WireMessage (RLPArray [RLPNumber 0x16]) = GetTransactions

obj2WireMessage (RLPArray x) = error ("Missing message: " ++ show x)


wireMessage2Obj::Message->RLPObject
wireMessage2Obj Hello { version = v,
                        clientId = cId,
                        capability = cap,
                        port = p,
                        nodeId = nId } =
  RLPArray [
    RLPNumber 0x00,
    RLPNumber v,
    RLPNumber 0,
    RLPString cId,
    RLPNumber $ fromIntegral $ foldl (.|.) 0x00 $ capValue <$> cap,
    RLPNumber p,
    word5122RLP nId
    ]
wireMessage2Obj Ping = RLPArray $ [RLPNumber 0x2]
wireMessage2Obj Pong = RLPArray $ [RLPNumber 0x3]
wireMessage2Obj GetPeers = RLPArray $ [RLPNumber 0x10]
wireMessage2Obj (Peers peers) = RLPArray $ (RLPNumber 0x11:(peer2RLP <$> peers))
wireMessage2Obj (Transactions transactions) =
  RLPArray (RLPNumber 0x12:(transaction2RLP <$> transactions))
  where
    transaction2RLP t =
      RLPArray [
        rlpNumber $ tNonce t,
        rlpNumber $ gasPrice t,
        RLPNumber $ tGasLimit t,
        address2RLP $ to t,
        rlpNumber $ value t,
        rlpNumber $ tInit t,
        RLPNumber $ fromIntegral $ v t,
        rlpNumber $ r t,
        rlpNumber $ s t
        ]
wireMessage2Obj (Blocks blocks) = error "Blocks missing in wireMessage2Obj"
wireMessage2Obj (GetChain pSHAs numChildren) = 
  RLPArray $ [RLPNumber 0x14] ++
  (sha2RLP <$> pSHAs) ++
  [rlpNumber numChildren]
wireMessage2Obj (NotInChain blocks) = error "NotInChain missing in wireMessage2Obj"
wireMessage2Obj GetTransactions = RLPArray [RLPNumber 0x16]

    
