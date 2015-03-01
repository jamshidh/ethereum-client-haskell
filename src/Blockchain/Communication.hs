
module Blockchain.Communication (
  sendMessage
  ) where

import Control.Monad.IO.Class
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.IO

import Blockchain.Data.RLP

import Blockchain.Context
import Blockchain.Display
import Blockchain.Data.Wire

ethereumHeader::B.ByteString->Put
ethereumHeader payload = do
  putWord32be 0x22400891
  putWord32be $ fromIntegral $ B.length payload
  putByteString payload

    

sendCommand::Handle->B.ByteString->IO ()
sendCommand handle payload = do
  let theData2 = runPut $ ethereumHeader payload
  BL.hPut handle theData2

sendMessage::Handle->Message->ContextM ()
sendMessage handle msg = do
  displayMessage True msg
  liftIO $ sendCommand handle $ rlpSerialize $ wireMessage2Obj msg
