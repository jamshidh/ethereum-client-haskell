
module DBs (
            DB(..),
            openDBs,
            StateDB,
            DetailsDB,
            BlockDB,
            SHAPtr(..)
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

type BlockDB = DB.DB
type DetailsDB = DB.DB
type StateDB = DB.DB

data DB = DB { blockDB::BlockDB, detailsDB::DetailsDB, stateDB::StateDB }

newtype SHAPtr = SHAPtr B.ByteString deriving (Show, Eq)

instance Format SHAPtr where
  format (SHAPtr x) = yellow $ format x

instance Binary SHAPtr where
  put (SHAPtr x) = do
      sequence_ $ put <$> B.unpack x
  get = do
    error "get undefined for SHAPtr"

instance RLPSerializable SHAPtr where
    rlpEncode (SHAPtr x) = rlpEncode x
    rlpDecode x = SHAPtr $ rlpDecode x

options::DB.Options
options = DB.defaultOptions {
  DB.createIfMissing=True, DB.cacheSize=1024}

openDBs::ResourceT IO DB
openDBs = do
  homeDir <- liftIO $ getHomeDirectory                     
  bdb <- DB.open (homeDir ++ blockDBPath) options
  ddb <- DB.open (homeDir ++ detailsDBPath) options
  sdb <- DB.open (homeDir ++ stateDBPath) options
  return $ DB bdb ddb sdb
