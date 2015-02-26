
module Blockchain.VM.Code where

import qualified Data.ByteString as B
import Network.Haskoin.Internals
import Numeric
import Text.PrettyPrint.ANSI.Leijen

import Blockchain.Data.RLP
import Blockchain.Format
import Blockchain.Util
import Blockchain.VM.Opcodes

data Code =
  Code {
    codeBytes::B.ByteString,
    validJumpDests::[Word256]
    } deriving (Show, Eq)



getOperationAt::Code->Word256->(Operation, Word256)
getOperationAt (Code rom _) p = getOperationAt' rom p

getOperationAt'::B.ByteString->Word256->(Operation, Word256)
getOperationAt' rom p = opCode2Op $ safeDrop p rom

showCode::Word256->Code->String
showCode _ (Code rom _) | B.null rom = ""
showCode lineNumber c@(Code rom jds) = showHex lineNumber "" ++ " " ++ format (B.pack $ op2OpCode op) ++ " " ++ show (pretty op) ++ "\n" ++  showCode (lineNumber + nextP) (Code (safeDrop nextP rom) jds)
        where
          (op, nextP) = getOperationAt c 0

instance Pretty Code where
    pretty = text . showCode 0

instance RLPSerializable Code where
    rlpEncode (Code rom _) = rlpEncode rom
    rlpDecode = bytes2Code . rlpDecode

getValidJUMPDESTs::B.ByteString->[Word256]
getValidJUMPDESTs bytes =
  map fst $ filter ((== JUMPDEST) . snd) $ getOps bytes 0
  where
    getOps::B.ByteString->Word256->[(Word256, Operation)]
    getOps bytes p | p > fromIntegral (B.length bytes) = []
    getOps code p = (p, op):getOps code (p+len)
      where
        (op, len) = getOperationAt' code p


codeLength::Code->Int
codeLength (Code c _) = B.length c

compile::[Operation]->Code
compile x = bytes2Code bytes
  where
    bytes = B.pack $ op2OpCode =<< x

bytes2Code::B.ByteString->Code
bytes2Code bytes = Code bytes $ getValidJUMPDESTs bytes
