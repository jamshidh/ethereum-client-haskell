
module VM.Code where

import qualified Data.ByteString as B
import Text.PrettyPrint.ANSI.Leijen

import Data.RLP
import VM.Opcodes

newtype Code = Code B.ByteString deriving (Show)



getOperationAt::Code->Int->(Operation, Int)
getOperationAt (Code rom) p = opCode2Op $ B.drop p rom

instance Pretty Code where
    pretty (Code rom) | B.null rom = empty
    pretty c@(Code rom) = text (show op ++ "\n") <> pretty (Code $ B.drop nextP rom)
        where
          (op, nextP) = getOperationAt c 0

instance RLPSerializable Code where
    rlpEncode (Code rom) = rlpEncode rom
    rlpDecode = Code . rlpDecode

codeLength::Code->Int
codeLength (Code c) = B.length c

compile::[Operation]->Code
compile x = Code $ B.pack $ op2OpCode =<< x
