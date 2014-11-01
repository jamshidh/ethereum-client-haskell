
module DBs where

import qualified Data.ByteString as B
import qualified Database.LevelDB as DB

type BlockDB = DB.DB
type DetailsDB = DB.DB
type StateDB = DB.DB

newtype SHAPtr = SHAPtr B.ByteString deriving (Show, Eq)
