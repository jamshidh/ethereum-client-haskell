
module Blockchain.Context (
  Context(..),
  ContextM,
  initContext,
  isDebugEnabled
  ) where


import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import System.Directory
import System.FilePath
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Constants
import Blockchain.DBM
import Blockchain.Data.Peer
import Blockchain.SHA

--import Debug.Trace

data Context =
  Context {
    neededBlockHashes::[SHA],
    pingCount::Int,
    peers::[Peer],
    debugEnabled::Bool
    }

type ContextM = StateT Context DBM

isDebugEnabled::ContextM Bool
isDebugEnabled = do
  cxt <- get
  return $ debugEnabled cxt 

initContext::String->ResourceT IO Context
initContext theType = do
  homeDir <- liftIO getHomeDirectory                     
  liftIO $ createDirectoryIfMissing False $ homeDir </> dbDir theType
  return $ Context
      []
      0
      []
      False
