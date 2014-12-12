
module Communication (
  sendMessage
  ) where

import Control.Monad.IO.Class
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Simple.TCP

import Data.RLP

import Context
import Display
import Data.Wire

ethereumHeader::B.ByteString->Put
ethereumHeader payload = do
  putWord32be 0x22400891
  putWord32be $ fromIntegral $ B.length payload
  putByteString payload

    

sendCommand::Socket->B.ByteString->IO ()
sendCommand socket payload = do
  let theData2 = runPut $ ethereumHeader payload
  send socket $ B.concat $ BL.toChunks theData2

sendMessage::Socket->Message->ContextM ()
sendMessage socket msg = do
  displayMessage True msg
  liftIO $ sendCommand socket $ rlpSerialize $ wireMessage2Obj msg
