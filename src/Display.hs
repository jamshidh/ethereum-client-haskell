
module Display (
  addPingCount,
  setPeers,
  displayMessage
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State

import qualified Colors as CL
import Context
import Data.Peer
import Format
import Data.Wire

setTitle::String->IO()
setTitle value = do
  putStr $ "\ESC]0;" ++ value ++ "\007"

updateStatus::ContextM ()
updateStatus = do
  cxt <- get
  liftIO $ setTitle $
    "pingCount = " ++ show (pingCount cxt)
    ++ ", peer count=" ++ show (length $ peers cxt)

addPingCount::ContextM ()
addPingCount = do
  cxt <- get
  put cxt{pingCount = pingCount cxt + 1}
  updateStatus

setPeers::[Peer]->ContextM ()
setPeers p = do
  cxt <- get
  put cxt{peers = p}
  updateStatus
  
displayMessage::Bool->Message->IO ()
displayMessage _ GetPeers = return ()
displayMessage _ (Peers _) = return ()
displayMessage outbound msg =
  if outbound
  then putStrLn (CL.red "msg<<<<: " ++ format msg)
  else putStrLn (CL.green "msg>>>>>: " ++ format msg)
       
