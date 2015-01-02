
module RLP
    (
     doit
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Default
import qualified Database.LevelDB as DB
import System.Directory
import System.Environment
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Data.RLP

import DumpLevelDB

--import Debug.Trace

doit::String->String->IO ()
doit dbtype h = do
  let options = DB.defaultOptions {
        DB.createIfMissing=True, DB.cacheSize=1024}
  dbDir <- typeToDB dbtype
  runResourceT $ do
    db <- DB.open (dbDir </> "blocks") def
    showAllKeyVal db (show . pretty . rlpDeserialize)






