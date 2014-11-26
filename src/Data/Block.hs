{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}

module Data.Block (
  BlockData(..),
  Block(..),
  blockHash,
  powFunc,
  headerHashWithoutNonce,
  addNonceToBlock,
  findNonce,
  fastFindNonce,
  nonceIsValid,
  genesisBlock
  ) where

import qualified Crypto.Hash.SHA3 as C
import qualified Data.Binary as DB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Base16
import Data.ByteString.Internal
import Data.Functor
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import Foreign
import Foreign.ForeignPtr.Unsafe
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Data.Address
import qualified Colors as CL
import Database.MerklePatricia
import Format
import Data.RLP
import SHA
import Data.TransactionReceipt
import Util

--import Debug.Trace

data BlockData = BlockData {
  parentHash::SHA,
  unclesHash::SHA,
  coinbase::Address,
  bStateRoot::SHAPtr,
  transactionsTrie::Integer,
  difficulty::Integer,
  number::Integer,
  minGasPrice::Integer,
  gasLimit::Integer,
  gasUsed::Integer,
  timestamp::UTCTime,
  extraData::Integer,
  nonce::SHA
} deriving (Show)

data Block = Block {
  blockData::BlockData,
  receiptTransactions::[TransactionReceipt],
  blockUncles::[BlockData]
  } deriving (Show)

instance Format Block where
  format b@Block{blockData=bd, receiptTransactions=receipts, blockUncles=[]} =
    CL.blue ("Block #" ++ show (number bd)) ++ " " ++
    tab (format (blockHash b) ++ "\n" ++
         format bd ++
         (if null receipts
          then "        (no transactions, no uncles)"
          else tab (intercalate "\n    " (format <$> receipts))))
  format _ = 
    error "format for Block not defined yet for blockUncles /= []."
              
instance RLPSerializable Block where
  rlpDecode (RLPArray [bd, RLPArray transactionReceipts, RLPArray []]) =
    Block (rlpDecode bd) (rlpDecode <$> transactionReceipts) []
  rlpDecode (RLPArray [_, RLPArray _, RLPArray _]) =
    error "rlpDecode for Block not defined yet for blockUncles /= []."
  rlpDecode (RLPArray arr) = error ("rlpDecode for Block called on object with wrong amount of data, length arr = " ++ show arr)
  rlpDecode x = error ("rlpDecode for Block called on non block object: " ++ show x)

  rlpEncode Block{blockData=bd, receiptTransactions=receipts, blockUncles=[]} =
    RLPArray [rlpEncode bd, RLPArray (rlpEncode <$> receipts), RLPArray []]
  rlpEncode _ = error "rlpEncode for Block not defined yet for blockUncles /= []."

instance RLPSerializable BlockData where
  rlpDecode (RLPArray [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13]) =
    BlockData {
      parentHash = rlpDecode v1,
      unclesHash = rlpDecode v2,
      coinbase = rlpDecode v3,
      bStateRoot = rlpDecode v4,
      transactionsTrie = rlpDecode v5,
      difficulty = rlpDecode v6,
      number = rlpDecode v7,
      minGasPrice = rlpDecode v8,
      gasLimit = rlpDecode v9,
      gasUsed = rlpDecode v10,
      timestamp = posixSecondsToUTCTime $ fromInteger $ rlpDecode v11,
      extraData = rlpDecode v12,
      nonce = rlpDecode v13
      }  
  rlpDecode (RLPArray arr) = error ("rlp2BlockData called on object with wrong amount of data, length arr = " ++ show arr)
  rlpDecode x = error ("rlp2BlockData called on non block object: " ++ show x)


  rlpEncode bd =
    RLPArray [
      rlpEncode $ parentHash bd,
      rlpEncode $ unclesHash bd,
      rlpEncode $ coinbase bd,
      rlpEncode $ bStateRoot bd,
      rlpEncode $ transactionsTrie bd,
      rlpEncode $ difficulty bd,
      rlpEncode $ number bd,
      rlpEncode $ minGasPrice bd,
      rlpEncode $ gasLimit bd,
      rlpEncode $ gasUsed bd,
      rlpEncode $ (round $ utcTimeToPOSIXSeconds $ timestamp bd::Integer),
      rlpEncode $ extraData bd,
      rlpEncode $ nonce bd
      ]

blockHash::Block->SHA
blockHash = hash . rlpSerialize . rlpEncode

instance Format BlockData where
  format b = 
    "parentHash: " ++ format (parentHash b) ++ "\n" ++
    "unclesHash: " ++ format (unclesHash b) ++ 
    (if unclesHash b == hash (B.pack [0xc0]) then " (the empty array)\n" else "\n") ++
    "coinbase: " ++ format (coinbase b) ++ "\n" ++
    "stateRoot: " ++ show (pretty $ bStateRoot b) ++ "\n" ++
    "transactionsTrie: " ++ show (transactionsTrie b) ++ "\n" ++
    "difficulty: " ++ show (difficulty b) ++ "\n" ++
    "minGasPrice: " ++ show (minGasPrice b) ++ "\n" ++
    "gasLimit: " ++ show (gasLimit b) ++ "\n" ++
    "gasUsed: " ++ show (gasUsed b) ++ "\n" ++
    "timestamp: " ++ show (timestamp b) ++ "\n" ++
    "extraData: " ++ show (extraData b) ++ "\n" ++
    "nonce: " ++ format (nonce b) ++ "\n"

genesisBlock::Block
genesisBlock =
  Block {
    blockData = 
       BlockData {
         parentHash = SHA 0,
         unclesHash = hash (B.pack [0xc0]), 
         coinbase = Address 0,
         bStateRoot = SHAPtr $ B.pack $ integer2Bytes 0x8dbd704eb38d1c2b73ee4788715ea5828a030650829703f077729b2b613dd206,
         transactionsTrie = 0,
         difficulty = 0x400000, --2^22,
         number = 0,
         minGasPrice = 0,
         gasLimit = 1000000,
         gasUsed = 0,
         timestamp = posixSecondsToUTCTime 0,
         extraData = 0,
         nonce = hash $ B.pack [42]
         },
    receiptTransactions=[],
    blockUncles=[]
    }

--------------------------
--Mining stuff

--used as part of the powFunc
noncelessBlockData2RLP::BlockData->RLPObject
noncelessBlockData2RLP bd =
  RLPArray [
      rlpEncode $ parentHash bd,
      rlpEncode $ unclesHash bd,
      rlpEncode $ coinbase bd,
      rlpEncode $ bStateRoot bd,
      rlpEncode $ transactionsTrie bd,
      rlpEncode $ difficulty bd,
      rlpEncode $ number bd,
      rlpEncode $ minGasPrice bd,
      rlpEncode $ gasLimit bd,
      rlpEncode $ gasUsed bd,
      rlpEncode $ (round $ utcTimeToPOSIXSeconds $ timestamp bd::Integer),
      rlpEncode $ extraData bd
      ]

{-
noncelessBlock2RLP::Block->RLPObject
noncelessBlock2RLP Block{blockData=bd, receiptTransactions=receipts, blockUncles=[]} =
  RLPArray [noncelessBlockData2RLP bd, RLPArray (rlpEncode <$> receipts), RLPArray []]
noncelessBlock2RLP _ = error "noncelessBock2RLP not definted for blockUncles /= []"
-}

sha2ByteString::SHA->B.ByteString
sha2ByteString (SHA val) = BL.toStrict $ DB.encode val

headerHashWithoutNonce::Block->ByteString
headerHashWithoutNonce b = C.hash 256 $ rlpSerialize $ noncelessBlockData2RLP $ blockData b

powFunc::Block->Integer
powFunc b =
  --trace (show $ headerHashWithoutNonce b) $
  byteString2Integer $ 
  C.hash 256 (
    headerHashWithoutNonce b
    `B.append`
    sha2ByteString (nonce $ blockData b))

nonceIsValid::Block->Bool
nonceIsValid b = powFunc b * (difficulty $ blockData b) < (2::Integer)^(256::Integer)

addNonceToBlock::Block->Integer->Block
addNonceToBlock b n =
  b {
    blockData=(blockData b) {nonce=SHA $ fromInteger n}
    }

findNonce::Block->Integer
findNonce b =
  case find (nonceIsValid . addNonceToBlock b) [1..] of
    Nothing -> error "Huh?  You ran out of numbers!!!!"
    Just n -> n

----------

fastFindNonce::Block->IO Integer
fastFindNonce b = do
  let (theData, _, _) = toForeignPtr $ headerHashWithoutNonce b
  let (theThreshold, _, _) = toForeignPtr threshold
  retValue <- mallocArray 32
  retInt <- c_fastFindNonce (unsafeForeignPtrToPtr theData) (unsafeForeignPtrToPtr theThreshold) retValue
  print retInt
  retData <- peekArray 32 retValue
  return $ byteString2Integer $ B.pack retData
  where
    threshold::B.ByteString
    threshold = fst $ decode $ BC.pack $ padZeros 64 $ showHex ((2::Integer)^(256::Integer) `quot` (difficulty $ blockData b)) ""

foreign import ccall "findNonce" c_fastFindNonce::Ptr Word8->Ptr Word8->Ptr Word8->IO Int
--foreign import ccall "fastFindNonce" c_fastFindNonce::ForeignPtr Word8->ForeignPtr Word8->ForeignPtr Word8


{-
fastFindNonce::Block->Integer
fastFindNonce b =
  byteString2Integer $ BC.pack $ 
  BC.unpack $
  C.hash 256 (
    first `B.append` second)
  where
    first = headerHashWithoutNonce b

fastPowFunc::Block->Integer
fastPowFunc b =
  --trace (show $ headerHashWithoutNonce b) $
  byteString2Integer $ BC.pack $ 
  BC.unpack $
  C.hash 256 (
    headerHashWithoutNonce b
    `B.append`
    sha2ByteString (nonce $ blockData b))
-}
