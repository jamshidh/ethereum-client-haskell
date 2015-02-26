
module Code
    (
     doit
    ) where

import Control.Monad.Trans.Resource
import Data.Default
import qualified Database.LevelDB as DB
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Data.RLP

import Blockchain.VM.Code

import DumpLevelDB

import Blockchain.Format

--import Debug.Trace

formatCode::Code->String
formatCode = show . pretty

doit::String->String->IO ()
doit dbtype h = showKeyVal (formatCode . bytes2Code) dbtype "state" (if h == "-" then Nothing else Just h)






