
module DB.CodeDB (
               addCode,
               getCode
              ) where

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Context
import ExtDBs
import SHA

addCode::B.ByteString->ContextM ()
addCode = codeDBPut

getCode::SHA->ContextM (Maybe B.ByteString)
getCode theHash = 
  codeDBGet (BL.toStrict $ encode $ sha2SHAPtr theHash)
