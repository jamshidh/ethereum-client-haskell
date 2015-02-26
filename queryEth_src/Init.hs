
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



showInit::Transaction->String
showInit ContractCreationTX {tInit=tInit'} = show (pretty tInit') ++ "\n----\n" ++ show tInit' --(map formatSequence (decompile tInit'))
showInit MessageTX {} = ""


doit::String->String->IO ()
doit dbtype h = showKeyVal (intercalate "\n" . map (showInit . unsignedTransaction) . receiptTransactions . rlpDecode . rlpDeserialize) dbtype "blocks" (if h == "-" then Nothing else Just h)






