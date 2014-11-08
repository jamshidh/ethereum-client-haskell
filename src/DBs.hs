
module DBs (
            DB(..),
            openDBs,
            StateDB,
            DetailsDB,
            BlockDB,
            SHAPtr(..),
            sha2SHAPtr
           )where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Binary
import qualified Data.ByteString as B
import Data.Functor
import qualified Database.LevelDB as DB
import System.Directory

import Colors
import Constants
import Format
import RLP
import SHA
import Util

type BlockDB = DB.DB
type DetailsDB = DB.DB
type StateDB = DB.DB

data DB = DB { blockDB::BlockDB, detailsDB::DetailsDB, stateDB::StateDB, stateRoot::SHAPtr }

newtype SHAPtr = SHAPtr B.ByteString deriving (Show, Eq)

instance Format SHAPtr where
  format (SHAPtr x) = yellow $ format x

instance Binary SHAPtr where
  put (SHAPtr x) = do
      sequence_ $ put <$> B.unpack x
  get = do
    error "get undefined for SHAPtr"

sha2SHAPtr::SHA->SHAPtr
sha2SHAPtr (SHA x) = SHAPtr $ B.pack $ word256ToBytes x

instance RLPSerializable SHAPtr where
    rlpEncode (SHAPtr x) = rlpEncode x
    rlpDecode x = SHAPtr $ rlpDecode x

options::DB.Options
options = DB.defaultOptions {
  DB.createIfMissing=True, DB.cacheSize=1024}

openDBs::Bool->ResourceT IO DB
openDBs useCppDBs = do
  homeDir <- liftIO $ getHomeDirectory                     
  bdb <- DB.open (homeDir ++ dbDir useCppDBs ++ blockDBPath) options
  ddb <- DB.open (homeDir ++ dbDir useCppDBs ++ detailsDBPath) options
  sdb <- DB.open (homeDir ++ dbDir useCppDBs ++ stateDBPath) options
  return $ DB bdb ddb sdb undefined
