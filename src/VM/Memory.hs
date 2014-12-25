{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module VM.Memory (
  Memory(..),
  getSizeInBytes,
  getSizeInWords,
  getShow,
  mLoad,
  mLoad8,
  mLoadByteString,
  mStore,
  mStore8,
  mStoreByteString
  ) where

import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Functor
import Data.IORef
import Data.Word
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import ExtWord
import Util
import VM.VMState

getSizeInWords::Memory->IO Word256
getSizeInWords (Memory _ size) = (ceiling . (/ (32::Double)) . fromIntegral) <$> readIORef size

getSizeInBytes::Memory->IO Word256
getSizeInBytes (Memory _ size) = (1+) <$> readIORef size

--In this function I use the words "size" and "length" to mean 2 different things....
--"size" is the highest memory location used (as described in the yellowpaper).
--"length" is the IOVector length, which will often be larger than the size.
--Basically, to avoid resizing the vector constantly (which could be expensive),
--I keep something larger around until it fills up, then reallocate (something even
--larger).
setNewMaxSize::VMState->Word256->IO VMState
setNewMaxSize state newSize = do
  oldSize <- readIORef (mSize $ memory state)
  when (newSize > oldSize) $ do
    writeIORef (mSize $ memory state) newSize

  let gasCharge =
        if newSize > oldSize
        then fromInteger $ (ceiling $ fromIntegral newSize/(32::Double)) - (ceiling $ fromIntegral oldSize/(32::Double))
        else 0

  let oldLength = fromIntegral $ V.length (mVector $ memory state)
  state' <-
    if newSize > oldLength
      then do
        arr' <- V.grow (mVector $ memory state) $ fromIntegral $ 2*newSize
        forM_ [oldLength-1..2*oldLength-1] $ \p -> V.write arr' (fromIntegral p) 0
        return $ state{memory=(memory state){mVector = arr'}}
      else return state

  return state'{vmGasRemaining=vmGasRemaining state - gasCharge}

getShow::Memory->IO String
getShow (Memory arr sizeRef) = do
  msize <- readIORef sizeRef
  --fmap (show . B16.encode . B.pack) $ sequence $ V.read arr <$> fromIntegral <$> [0..fromIntegral msize-1] 
  fmap (show . B16.encode . B.pack) $ sequence $ V.read arr <$> [0..fromIntegral msize-1] 


mLoad::VMState->Word256->IO [Word8]
mLoad state p = do
  --setNewMaxSize m (p+31)
  sequence $ V.read (mVector $ memory state) <$> fromIntegral <$> [p..p+31] 

mLoad8::VMState->Word256->IO Word8
mLoad8 state p = do
  --setNewMaxSize m p
  V.read (mVector $ memory state) (fromIntegral p)

mLoadByteString::VMState->Word256->Word256->IO B.ByteString
mLoadByteString state p size = do
  --setNewMaxSize m (p+size)
  fmap B.pack $ sequence $ V.read (mVector $ memory state) <$> fromIntegral <$> [p..p+size-1] 


mStore::VMState->Word256->Word256->IO VMState
mStore state p val = do
  state' <- setNewMaxSize state (p+32)
  sequence_ $ uncurry (V.write $ mVector $ memory state') <$> zip [fromIntegral p..] (word256ToBytes val)
  return state'

mStore8::VMState->Word256->Word8->IO VMState
mStore8 state p val = do
  state' <- setNewMaxSize state p
  V.write (mVector $ memory state') (fromIntegral p) val
  return state'

mStoreByteString::VMState->Word256->B.ByteString->IO VMState
mStoreByteString state p theData = do
  state' <- setNewMaxSize state (p + fromIntegral (B.length theData))
  sequence_ $ uncurry (V.write $ mVector $ memory state') <$> zip (fromIntegral <$> [p..p+fromIntegral (B.length theData)]) (B.unpack theData)
  return state'

