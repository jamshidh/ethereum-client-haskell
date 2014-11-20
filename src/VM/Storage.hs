
module VM.Storage (
  addStorageToDB
  ) where

import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.Map as M

import DB.DBs
import DB.EthDB
import ExtWord
import DB.ModifyStateDB
import qualified Data.NibbleString as N
import Data.RLP
import Util

type Storage = M.Map Word256 Word256

addItem::DB->(N.NibbleString, RLPObject)->ResourceT IO DB
addItem db (key, val) = putKeyVal db key val

addItems::DB->[(N.NibbleString, RLPObject)]->ResourceT IO DB
addItems db [] = return db
addItems db (x:rest) = do
  db' <- addItem db x
  addItems db' rest

addStorageToDB::DB->Storage->ResourceT IO DB
addStorageToDB db storage = do
  liftIO $ print storage
  db' <- initializeBlankStateDB db
  addItems db' $ map (N.pack . (N.byte2Nibbles =<<) . word256ToBytes *** rlpEncode . rlpSerialize . rlpEncode . toInteger) $ M.toList storage
