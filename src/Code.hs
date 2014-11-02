
module Code where

import qualified Data.ByteString as B

import Format
import Opcodes
import RLP

newtype Code = Code B.ByteString deriving (Show)



getOperationAt::Code->Int->(Operation, Int)
getOperationAt (Code rom) p = opCode2Op $ B.drop p rom

instance Format Code where
    format (Code rom) | B.null rom = ""
    format c@(Code rom) = show op ++ "\n" ++ format (Code $ B.drop nextP rom)
        where
          (op, nextP) = getOperationAt c 0

instance RLPSerializable Code where
    rlpEncode (Code rom) = rlpEncode rom
    rlpDecode = Code . rlpDecode

codeLength::Code->Int
codeLength (Code c) = B.length c
