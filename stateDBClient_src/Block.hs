
module Block 
    (
     doit
    ) where

--import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
--import qualified Data.ByteString as B
--import qualified Data.ByteString.Base16 as B16
--import qualified Data.ByteString.Char8 as BC
import Data.Default
import qualified Database.LevelDB as DB
--import System.Directory
--import System.Environment
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))
--
import Data.RLP
--
import Data.Block
--
import DumpLevelDB

import Format

--import Debug.Trace

formatBlock::Block->String
formatBlock = format

doit::String->String->IO ()
doit dbtype h = showKeyVal (formatBlock . rlpDecode . rlpDeserialize) dbtype "blocks" (if h == "-" then Nothing else Just h)






