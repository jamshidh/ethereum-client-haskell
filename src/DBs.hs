
module DBs where

import Data.Binary
import qualified Data.ByteString as B
import Data.Functor
import qualified Database.LevelDB as DB

import Colors
import Format
import RLP

type BlockDB = DB.DB
type DetailsDB = DB.DB
type StateDB = DB.DB

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

