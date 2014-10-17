
module DBs where

import qualified Database.LevelDB as DB

type BlockDB = DB.DB
type DetailsDB = DB.DB
type StateDB = DB.DB
