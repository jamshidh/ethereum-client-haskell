
module Blockchain.VM.Code where

import qualified Data.ByteString as B
import Network.Haskoin.Internals
import Numeric
import Text.PrettyPrint.ANSI.Leijen

import Blockchain.Data.Code
import Blockchain.Format
import Blockchain.Util
import Blockchain.VM.Opcodes


getOperationAt::Code->Word256->(Operation, Word256)
getOperationAt (Code bytes) p = getOperationAt' bytes p

getOperationAt'::B.ByteString->Word256->(Operation, Word256)
getOperationAt' rom p = opCode2Op $ safeDrop p rom

showCode::Word256->Code->String
showCode _ (Code bytes) | B.null bytes = ""
showCode lineNumber c@(Code rom) = showHex lineNumber "" ++ " " ++ format (B.pack $ op2OpCode op) ++ " " ++ show (pretty op) ++ "\n" ++  showCode (lineNumber + nextP) (Code (safeDrop nextP rom))
        where
          (op, nextP) = getOperationAt c 0

instance Pretty Code where
    pretty = text . showCode 0

getValidJUMPDESTs::Code->[Word256]
getValidJUMPDESTs (Code bytes) =
  map fst $ filter ((== JUMPDEST) . snd) $ getOps bytes 0
  where
    getOps::B.ByteString->Word256->[(Word256, Operation)]
    getOps bytes' p | p > fromIntegral (B.length bytes') = []
    getOps code p = (p, op):getOps code (p+len)
      where
        (op, len) = getOperationAt' code p


codeLength::Code->Int
codeLength (Code bytes) = B.length bytes

compile::[Operation]->Code
compile x = Code bytes
  where
    bytes = B.pack $ op2OpCode =<< x

