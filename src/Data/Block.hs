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
  getBlock,
  putBlock
  ) where

import qualified Crypto.Hash.SHA3 as C
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Internal
import Data.Functor
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Foreign
import Foreign.ForeignPtr.Unsafe
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Context
import Data.Address
import qualified Colors as CL
import Database.MerklePatricia
import ExtDBs
import Format
import Data.RLP
import SHA
import Data.SignedTransaction
import Util

--import Debug.Trace

data BlockData = BlockData {
  parentHash::SHA,
  unclesHash::SHA,
  coinbase::Address,
  bStateRoot::SHAPtr,
  transactionsRoot::SHAPtr,
  receiptsRoot::SHAPtr,
  logBloom::B.ByteString,
  difficulty::Integer,
  number::Integer,
  gasLimit::Integer,
  gasUsed::Integer,
  timestamp::UTCTime,
  extraData::Integer,
  nonce::SHA
} deriving (Show)

data Block = Block {
  blockData::BlockData,
  receiptTransactions::[SignedTransaction],
  blockUncles::[BlockData]
  } deriving (Show)

instance Format Block where
  format b@Block{blockData=bd, receiptTransactions=receipts, blockUncles=uncles} =
    CL.blue ("Block #" ++ show (number bd)) ++ " " ++
    tab (show (pretty $ blockHash b) ++ "\n" ++
         format bd ++
         (if null receipts
          then "        (no transactions)\n"
          else tab (intercalate "\n    " (format <$> receipts))) ++
         (if null uncles
          then "        (no uncles)"
          else tab ("Uncles:" ++ tab ("\n" ++ intercalate "\n    " (format <$> uncles)))))
              
instance RLPSerializable Block where
  rlpDecode (RLPArray [bd, RLPArray transactionReceipts, RLPArray uncles]) =
    Block (rlpDecode bd) (rlpDecode <$> transactionReceipts) (rlpDecode <$> uncles)
  rlpDecode (RLPArray arr) = error ("rlpDecode for Block called on object with wrong amount of data, length arr = " ++ show arr)
  rlpDecode x = error ("rlpDecode for Block called on non block object: " ++ show x)

  rlpEncode Block{blockData=bd, receiptTransactions=receipts, blockUncles=uncles} =
    RLPArray [rlpEncode bd, RLPArray (rlpEncode <$> receipts), RLPArray $ rlpEncode <$> uncles]

instance RLPSerializable BlockData where
  rlpDecode (RLPArray [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14]) =
    BlockData {
      parentHash = rlpDecode v1,
      unclesHash = rlpDecode v2,
      coinbase = rlpDecode v3,
      bStateRoot = rlpDecode v4,
      transactionsRoot = rlpDecode v5,
      receiptsRoot = rlpDecode v6,
      logBloom = rlpDecode v7,
      difficulty = rlpDecode v8,
      number = rlpDecode v9,
      gasLimit = rlpDecode v10,
      gasUsed = rlpDecode v11,
      timestamp = posixSecondsToUTCTime $ fromInteger $ rlpDecode v12,
      extraData = rlpDecode v13,
      nonce = rlpDecode v14
      }  
  rlpDecode (RLPArray arr) = error ("Error in rlpDecode for Block: wrong number of items, expected 14, got " ++ show (length arr) ++ ", arr = " ++ show (pretty arr))
  rlpDecode x = error ("rlp2BlockData called on non block object: " ++ show x)


  rlpEncode bd =
    RLPArray [
      rlpEncode $ parentHash bd,
      rlpEncode $ unclesHash bd,
      rlpEncode $ coinbase bd,
      rlpEncode $ bStateRoot bd,
      rlpEncode $ transactionsRoot bd,
      rlpEncode $ receiptsRoot bd,
      rlpEncode $ logBloom bd,
      rlpEncode $ difficulty bd,
      rlpEncode $ number bd,
      rlpEncode $ gasLimit bd,
      rlpEncode $ gasUsed bd,
      rlpEncode (round $ utcTimeToPOSIXSeconds $ timestamp bd::Integer),
      rlpEncode $ extraData bd,
      rlpEncode $ nonce bd
      ]

blockHash::Block->SHA
blockHash (Block info _ _) = hash . rlpSerialize . rlpEncode $ info

instance Format BlockData where
  format b = 
    "parentHash: " ++ show (pretty
                            $ parentHash b) ++ "\n" ++
    "unclesHash: " ++ show (pretty $ unclesHash b) ++ 
    (if unclesHash b == hash (B.pack [0xc0]) then " (the empty array)\n" else "\n") ++
    "coinbase: " ++ show (pretty $ coinbase b) ++ "\n" ++
    "stateRoot: " ++ show (pretty $ bStateRoot b) ++ "\n" ++
    "transactionsRoot: " ++ show (pretty $ transactionsRoot b) ++ "\n" ++
    "receiptsRoot: " ++ show (pretty $ receiptsRoot b) ++ "\n" ++
    "difficulty: " ++ show (difficulty b) ++ "\n" ++
    "gasLimit: " ++ show (gasLimit b) ++ "\n" ++
    "gasUsed: " ++ show (gasUsed b) ++ "\n" ++
    "timestamp: " ++ show (timestamp b) ++ "\n" ++
    "extraData: " ++ show (pretty $ extraData b) ++ "\n" ++
    "nonce: " ++ show (pretty $ nonce b) ++ "\n"

getBlock::SHA->ContextM (Maybe Block)
getBlock h = 
  fmap (rlpDecode . rlpDeserialize) <$> blockDBGet (BL.toStrict $ encode h)

putBlock::Block->ContextM ()
putBlock b = do
  let bytes = rlpSerialize $ rlpEncode b
  blockDBPut (BL.toStrict $ encode $ blockHash b) bytes


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
      rlpEncode $ transactionsRoot bd,
      rlpEncode $ receiptsRoot bd,
      rlpEncode $ logBloom bd,
      rlpEncode $ difficulty bd,
      rlpEncode $ number bd,
      rlpEncode $ gasLimit bd,
      rlpEncode $ gasUsed bd,
      rlpEncode (round $ utcTimeToPOSIXSeconds $ timestamp bd::Integer),
      rlpEncode $ extraData bd
      ]

{-
noncelessBlock2RLP::Block->RLPObject
noncelessBlock2RLP Block{blockData=bd, receiptTransactions=receipts, blockUncles=[]} =
  RLPArray [noncelessBlockData2RLP bd, RLPArray (rlpEncode <$> receipts), RLPArray []]
noncelessBlock2RLP _ = error "noncelessBock2RLP not definted for blockUncles /= []"
-}

sha2ByteString::SHA->B.ByteString
sha2ByteString (SHA val) = BL.toStrict $ encode val

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
nonceIsValid b = powFunc b * difficulty (blockData b) < (2::Integer)^(256::Integer)

addNonceToBlock::Block->Integer->Block
addNonceToBlock b n =
  b {
    blockData=(blockData b) {nonce=SHA $ fromInteger n}
    }

findNonce::Block->Integer
findNonce b =
    fromMaybe (error "Huh?  You ran out of numbers!!!!") $
              find (nonceIsValid . addNonceToBlock b) [1..]


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
    threshold = fst $ B16.decode $ BC.pack $ padZeros 64 $ showHex ((2::Integer)^(256::Integer) `quot` difficulty (blockData b)) ""

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
