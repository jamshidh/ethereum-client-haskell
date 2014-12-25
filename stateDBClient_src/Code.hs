
module Code
    (
     doit
    ) where

import Control.Monad.Trans.Resource
import Data.Default
import qualified Database.LevelDB as DB
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Data.RLP

import VM.Code

import DumpLevelDB

import Format

--import Debug.Trace

formatCode::Code->String
formatCode = show . pretty

doit::String->String->IO ()
doit dbtype h = showKeyVal (formatCode . Code) dbtype "state" (if h == "-" then Nothing else Just h)






