
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

getCode::Maybe SHA->ContextM (Maybe B.ByteString)
getCode Nothing = return $ Just B.empty 
getCode (Just theHash) = 
  codeDBGet (BL.toStrict $ encode $ sha2SHAPtr theHash)
