
module Init
    (
     doit
    ) where

import Control.Monad.Trans.Resource
import Data.Default
import qualified Database.LevelDB as DB
import Data.List
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Data.RLP

import Blockchain.Data.Block
import Blockchain.Data.SignedTransaction
import Blockchain.Data.Transaction
import Blockchain.Data.TransactionReceipt
import Blockchain.Util
import Blockchain.VM.Code
import Blockchain.VM.Opcodes

import Decompile1
import DumpLevelDB

import Blockchain.Format

--import Debug.Trace

formatBlock::Block->String
formatBlock = format

data StackElement = StackNumber Int | ItemDescription String deriving (Show)

stackElementSum::StackElement->StackElement->String
stackElementSum (StackNumber x) (StackNumber y) = show $ x+y

formatSE::StackElement->String
formatSE (StackNumber x) = show x
formatSE (ItemDescription s) = s

decompile'::[StackElement]->Int->Code->String
decompile' stack p c = 
    case (op, stack) of
      (STOP, _) -> "STOP"
      (RETURN, [memP, memSize]) -> "RETURN Mem[" ++ formatSE memP ++ ":" ++ show (memP `stackElementSum` memSize) ++ "]"
      (CALLER, _) -> decompile' (ItemDescription "$CALLER":stack) (p+size) c
      (GAS, _) -> decompile' (ItemDescription "$GAS":stack) (p+size) c
      (POP, _:rest) -> decompile' rest (p+size) c
      (SWAP1, first:second:rest) -> decompile' (second:first:rest) (p+size) c
      (DUP1, first:rest) -> decompile' (first:first:rest) (p+size) c
      (DUP2, first:second:rest) -> decompile' (second:first:second:rest) (p+size) c
      (DUP3, v1:i1:v2:rest) -> decompile' (v2:i1:v1:rest) (p+size) c
      (DUP4, v1:i1:i2:v2:rest) -> decompile' (v2:i1:i2:v1:rest) (p+size) c
      (DUP5, v1:i1:i2:i3:v2:rest) -> decompile' (v2:i1:i2:i3:v1:rest) (p+size) c
      (DUP6, v1:i1:i2:i3:i4:v2:rest) -> decompile' (v2:i1:i2:i3:i4:v1:rest) (p+size) c
      (EXP, v1:v2:rest) -> decompile' (ItemDescription (formatSE v1 ++ "^" ++ formatSE v2):rest) (p+size) c
      (SUB, v1:v2:rest) -> decompile' (ItemDescription (formatSE v1 ++ "-" ++ formatSE v2):rest) (p+size) c
      (AND, v1:v2:rest) -> decompile' (ItemDescription (formatSE v1 ++ " AND " ++ formatSE v2):rest) (p+size) c

      (PUSH val, _) -> decompile' (StackNumber (fromInteger $ bytes2Integer val):stack) (p+size) c
      (SSTORE, (key:val:rest)) -> "STORE[" ++ formatSE key ++ "] = " ++ formatSE val ++ "\n" ++ decompile' rest (p+size) c
      (MLOAD, (memP:rest)) -> decompile' (ItemDescription ("Mem[" ++ formatSE memP ++ "]"):rest) (p+size) c
      (CALLDATALOAD, (dataP:rest)) -> decompile' (ItemDescription ("Data[" ++ formatSE dataP ++ "]"):rest) (p+size) c
      (MSTORE, (memP:val:rest)) -> "Mem[" ++ formatSE memP ++ "] = " ++ formatSE val ++ "\n" ++ decompile' rest (p+size) c
      (CODECOPY, memP:codeP:codeSize:rest) -> "CODECOPY from CODE[" ++ formatSE codeP ++ ":" ++ show (codeP `stackElementSum` codeSize) ++ "] to Mem[" ++ formatSE memP ++ ":" ++ show (memP `stackElementSum` codeSize) ++ "]\n" ++ decompile' rest (p+size) c
      (CALL, gas:to:value:inOffset:inSize:outOffset:outSize:rest) -> "CALL[" ++ formatSE to ++ "] (gas=" ++ formatSE gas ++ ", value=" ++ formatSE value ++ ", inOffset = " ++ formatSE inOffset ++ ", inSize=" ++ formatSE inSize ++ ", outOffset=" ++ formatSE outOffset ++ ", outSize=" ++ formatSE outSize ++ ")\n" ++ decompile' (ItemDescription "<call result>":rest) (p+size) c
      (CALL, stack) -> "CALL messup: " ++ show stack
      (CALLCODE, gas:to:value:inOffset:inSize:outOffset:outSize:rest) -> "CALLCODE[" ++ formatSE to ++ "] (gas=" ++ formatSE gas ++ ", value=" ++ formatSE value ++ ", inOffset = " ++ formatSE inOffset ++ ", inSize=" ++ formatSE inSize ++ ", outOffset=" ++ formatSE outOffset ++ ", outSize=" ++ formatSE outSize ++ ")\n" ++ decompile' rest (p+size) c
      (CALLCODE, stack) -> "CALLCODE messup: " ++ show stack
      (JUMP, StackNumber location:rest) -> "JUMP to " ++ show location ++ "\n" ++ decompile' rest location c
      (JUMP, ItemDescription location:rest) -> "JUMP to " ++ show location ++ "\n" ++ decompile' rest (p+size) c
      (JUMP, _) -> "JUMP messup: " ++ show stack
      _ -> show op ++ "\n" ++ decompile' stack (p+size) c
    where
      (op, size) = getOperationAt c p


showInit::Transaction->String
showInit ContractCreationTX {tInit=tInit'} = show (pretty tInit') ++ "\n----\n" ++ intercalate "===========" (map show (decompile tInit')) -- (map formatSequence (decompile tInit'))
showInit MessageTX {} = ""


doit::String->String->IO ()
doit dbtype h = showKeyVal (intercalate "\n" . map (showInit . unsignedTransaction) . receiptTransactions . rlpDecode . rlpDeserialize) dbtype "blocks" (if h == "-" then Nothing else Just h)






