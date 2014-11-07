
module Storage (
  addStorageToDB
  ) where

import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.Map as M

import DBs
import EthDB
import ExtWord
import ModifyStateDB
import qualified NibbleString as N
import RLP
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
  addItems db' $ map (N.pack . (N.byte2Nibbles =<<) . word256ToBytes *** rlpEncode . toInteger) $ M.toList storage
