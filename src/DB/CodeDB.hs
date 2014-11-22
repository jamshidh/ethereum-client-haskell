
module DB.CodeDB (
               addCode,
               getCode
              ) where

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Context
import Database.MerklePatricia
import ExtDBs

addCode::B.ByteString->ContextM ()
addCode codeBytes = 
  codeDBPut codeBytes 

getCode::SHAPtr->ContextM (Maybe B.ByteString)
getCode theHash = 
  codeDBGet (BL.toStrict $ encode theHash)
