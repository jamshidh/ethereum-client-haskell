
module CodeDB (
               addCode,
               getCode
              ) where

import Control.Monad.Trans.Resource
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default
import qualified Database.LevelDB as DB

import DBs
import SHA

addCode::DB->B.ByteString->ResourceT IO ()
addCode db codeBytes = 
  DB.put (stateDB db) def (BL.toStrict $ encode $ hash codeBytes) codeBytes 

getCode::DB->SHAPtr->ResourceT IO (Maybe B.ByteString)
getCode db theHash = 
  DB.get (stateDB db) def (BL.toStrict $ encode theHash)
