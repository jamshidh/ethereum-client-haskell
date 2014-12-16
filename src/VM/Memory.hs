{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module VM.Memory (
  Memory(..),
  newMemory,
  getSize,
  getShow,
  mLoad,
  mLoad8,
  mLoadByteString,
  mStore,
  mStore8,
  mStoreByteString
  ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Functor
import Data.IORef
import Data.Word
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Context
import ExtWord
import Format
import Util

data Memory = Memory (V.IOVector Word8) (IORef Word256)


newMemory::IO Memory
newMemory = do
  arr <- V.new 100
  size <- newIORef 0
  forM [0..99] $ \p -> V.write arr (fromIntegral p) 0
  return $ Memory arr size

getSize::Memory->IO Word256
getSize (Memory _ size) = (ceiling . (/ (32::Double)) . fromIntegral) <$> readIORef size

setNewMaxSize::Memory->Word256->IO Memory
setNewMaxSize m@(Memory arr size) newVal = do
  oldVal <- readIORef size
  when (newVal > oldVal) $
    writeIORef size newVal
  let oldLength = fromIntegral $ V.length arr
  if newVal > oldLength
    then do
      arr' <- V.grow arr $ fromIntegral $ 2*newVal
      forM [oldLength-1..2*oldLength-1] $ \p -> V.write arr' (fromIntegral p) 0
      return $ Memory arr' size
    else return m

getShow::Memory->IO String
getShow (Memory arr sizeRef) = do
  msize <- readIORef sizeRef
  fmap (show . B16.encode . B.pack) $ sequence $ V.read arr <$> fromIntegral <$> [0..fromIntegral msize-1] 


mLoad::Memory->Word256->IO [Word8]
mLoad (Memory arr _) p = do
  --setNewMaxSize m (p+31)
  sequence $ V.read arr <$> fromIntegral <$> [p..p+31] 

mLoad8::Memory->Word256->IO Word8
mLoad8 (Memory arr _) p = do
  --setNewMaxSize m p
  V.read arr (fromIntegral p)

mLoadByteString::Memory->Word256->Word256->IO B.ByteString
mLoadByteString (Memory arr _) p size = do
  --setNewMaxSize m (p+size)
  fmap B.pack $ sequence $ V.read arr <$> fromIntegral <$> [p..p+size-1] 


mStore::Memory->Word256->Word256->IO Memory
mStore m p val = do
  m'@(Memory arr' _) <- setNewMaxSize m (p+32)
  sequence_ $ uncurry (V.write arr') <$> zip [fromIntegral p..] (word256ToBytes val)
  return m'

mStore8::Memory->Word256->Word8->IO Memory
mStore8 m p val = do
  m'@(Memory arr' _) <- setNewMaxSize m p
  V.write arr' (fromIntegral p) val
  return m'

mStoreByteString::Memory->Word256->B.ByteString->IO Memory
mStoreByteString m p theData = do
  m'@(Memory arr' _) <- setNewMaxSize m (p + fromIntegral (B.length theData))
  sequence_ $ uncurry (V.write arr') <$> zip (fromIntegral <$> [p..p+fromIntegral (B.length theData)]) (B.unpack theData)
  return m'

