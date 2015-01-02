
module DumpLevelDB 
    (
     showKeyVal,
     typeToDB
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

--import Debug.Trace

instance Pretty B.ByteString where
  pretty = blue . text . BC.unpack . B16.encode

showAllKeyVal::DB.DB->(B.ByteString->String)->ResourceT IO ()
showAllKeyVal db f = do
  i <- DB.iterOpen db def
  DB.iterFirst i
  valid <- DB.iterValid i
  if valid
    then showAllKeyVal' db i
    else liftIO $ putStrLn "no keys"
  where
    showAllKeyVal'::DB.DB->DB.Iterator->ResourceT IO ()
    showAllKeyVal' db' i = do
      Just key <- DB.iterKey i
      Just val <- DB.iterValue i
      if B.null val
        then liftIO $ putStrLn $ "----------\n" ++ show (pretty key) ++ ": <BLANK>"
        else liftIO $ putStrLn $ "----------\n" ++ show (pretty key) ++ ": " ++ f val
      DB.iterNext i
      v <- DB.iterValid i
      if v
        then showAllKeyVal' db' i
        else return ()

showKeyVal::(B.ByteString->String)->String->String->Maybe String->IO ()
showKeyVal f dbType dbName maybeKey = do
  let options = DB.defaultOptions {
        DB.createIfMissing=True, DB.cacheSize=1024}
  dbDir <- typeToDB dbType
  runResourceT $ do
    db <- DB.open (dbDir </> dbName) def
    case maybeKey of
      Nothing -> showAllKeyVal db f
      Just key -> do
                   maybeVal <- DB.get db def $ fst $ B16.decode $ BC.pack key
                   case maybeVal of
                     Nothing -> error $ "Missing value in database: " ++ show key
                     Just val -> liftIO $ putStrLn $ f val


typeToDB::String->IO String
typeToDB "h" = do
  homeDir <- getHomeDirectory
  return $ homeDir </> ".ethereumH"
typeToDB "c" = do
  homeDir <- getHomeDirectory
  return $ homeDir </> "Library" </> "Application Support" </> "Ethereum"

