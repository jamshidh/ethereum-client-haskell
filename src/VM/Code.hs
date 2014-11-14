
module VM.Code where

import qualified Data.ByteString as B

import Data.RLP
import ExtWord
import Format
import Util
import VM.Opcodes

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

addLoc::Word256->[Operation]->[Operation]
addLoc _ [] = []
addLoc p (RelativeLoc val:rest) = PUSH (integer2Bytes1 (fromIntegral $ p+val)):addLoc (p+1) rest
addLoc p (x:rest) = x:addLoc (p+opLen x) rest


compile::[Operation]->Code
compile x = Code $ B.pack $ op2OpCode =<< addLoc 0 x
