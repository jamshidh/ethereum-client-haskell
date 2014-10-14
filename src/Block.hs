{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}

module Block (
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

import Address
import Colors
import EthDB
import Format
import RLP
import SHA
import TransactionReceipt
import Util

--import Debug.Trace

data BlockData = BlockData {
  parentHash::SHA,
  unclesHash::SHA,
  coinbase::Address,
  stateRoot::SHAPtr,
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
    blue ("Block #" ++ show (number bd)) ++ " " ++
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
      coinbase = rlp2Address v3,
      stateRoot = rlpDecode v4,
      transactionsTrie = fromIntegral $ getNumber v5,
      difficulty = fromIntegral $ getNumber v6,
      number = fromIntegral $ getNumber v7,
      minGasPrice = fromIntegral $ getNumber v8,
      gasLimit = fromIntegral $ getNumber v9,
      gasUsed = fromIntegral $ getNumber v10,
      timestamp = posixSecondsToUTCTime $ fromIntegral $ getNumber v11,
      extraData = fromIntegral $ getNumber v12,
      nonce = rlpDecode v13
      }  
  rlpDecode (RLPArray arr) = error ("rlp2BlockData called on object with wrong amount of data, length arr = " ++ show arr)
  rlpDecode x = error ("rlp2BlockData called on non block object: " ++ show x)


  rlpEncode bd =
    RLPArray [
      rlpEncode $ parentHash bd,
      rlpEncode $ unclesHash bd,
      address2RLP $ coinbase bd,
      rlpEncode $ stateRoot bd,
      rlpNumber $ transactionsTrie bd,
      rlpNumber $ difficulty bd,
      rlpNumber $ number bd,
      rlpNumber $ minGasPrice bd,
      rlpNumber $ gasLimit bd,
      rlpNumber $ gasUsed bd,
      rlpNumber $ round $ utcTimeToPOSIXSeconds $ timestamp bd,
      rlpNumber $ extraData bd,
      rlpEncode $ nonce bd
      ]

blockHash::Block->SHA
blockHash = hash . B.pack . rlp2Bytes . rlpEncode

instance Format BlockData where
  format b = 
    "parentHash: " ++ format (parentHash b) ++ "\n" ++
    "unclesHash: " ++ format (unclesHash b) ++ 
    (if unclesHash b == hash (B.pack [0xc0]) then " (the empty array)\n" else "\n") ++
    "coinbase: " ++ format (coinbase b) ++ "\n" ++
    "stateRoot: " ++ format (stateRoot b) ++ "\n" ++
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
         stateRoot = SHAPtr $ B.pack $ integer2Bytes 0x8dbd704eb38d1c2b73ee4788715ea5828a030650829703f077729b2b613dd206,
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
      address2RLP $ coinbase bd,
      rlpEncode $ stateRoot bd,
      rlpNumber $ transactionsTrie bd,
      rlpNumber $ difficulty bd,
      rlpNumber $ number bd,
      rlpNumber $ minGasPrice bd,
      rlpNumber $ gasLimit bd,
      rlpNumber $ gasUsed bd,
      rlpNumber $ round $ utcTimeToPOSIXSeconds $ timestamp bd,
      rlpNumber $ extraData bd
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
headerHashWithoutNonce b = C.hash 256 $ B.pack $ rlp2Bytes $ noncelessBlockData2RLP $ blockData b

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
