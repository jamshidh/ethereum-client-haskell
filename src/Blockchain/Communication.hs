
module Blockchain.Communication (
  recvMsg,
  sendMsg
  ) where

import Control.Monad.Trans
import qualified Data.ByteString as B

import Blockchain.Data.RLP

import Blockchain.Context
import Blockchain.Display
import Blockchain.Data.Wire
import Blockchain.Frame

{-
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
  let (pType, pData) = wireMessage2Obj msg
  liftIO $ sendCommand handle $ B.cons pType $ rlpSerialize pData
-}

sendMsg::Message->EthCryptM ContextM ()
sendMsg msg = do
  lift $ displayMessage True msg
  let (pType, pData) = wireMessage2Obj msg
  encryptAndPutFrame $
    B.cons pType $ rlpSerialize pData

recvMsg::EthCryptM ContextM Message
recvMsg = do
  frameData <- getAndDecryptFrame

  let packetType = fromInteger $ rlpDecode $ rlpDeserialize $ B.take 1 frameData
      packetData = rlpDeserialize $ B.drop 1 frameData

  return $ obj2WireMessage packetType packetData
