{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module VM.Memory (
  Memory(..),
  newMemory,
  getSize,
  mLoad,
  mLoad8,
  mLoadByteString,
  mStore,
  mStore8,
  mStoreByteString
  ) where

import Data.Array.IO
import qualified Data.ByteString as B
import Data.Functor
import Data.IORef
import Data.Word

import ExtWord
import Util

data Memory = Memory (IOArray Word256 Word8) (IORef Word256)


newMemory::IO Memory
newMemory = do
  m <- newArray (0, 100) 0
  size <- newIORef 0
  return $ Memory m size

getSize::Memory->IO Word256
getSize (Memory _ size) = (ceiling . (/ (32::Double)) . fromIntegral) <$> readIORef size

setNewMaxSize::Memory->Word256->IO()
setNewMaxSize (Memory _ size) newVal = do
  putStrLn $ "newVal: " ++ show newVal
  oldVal <- readIORef size
  if newVal > oldVal
    then writeIORef size newVal
    else return ()
         
mLoad::Memory->Word256->IO [Word8]
mLoad m@(Memory arr _) p = do
  putStrLn "first"
  setNewMaxSize m (p+31)
  sequence $ readArray arr <$> [p..p+31] 

mLoad8::Memory->Word256->IO Word8
mLoad8 m@(Memory arr _) p = do
  putStrLn "second"
  setNewMaxSize m p
  readArray arr p

mLoadByteString::Memory->Word256->Word256->IO B.ByteString
mLoadByteString m@(Memory arr _) p size = do
  putStrLn $ "third: " ++ show p ++ ", " ++ show size
  setNewMaxSize m (p+size)
  fmap B.pack $ sequence $ readArray arr <$> [p..p+size-1] 


mStore::Memory->Word256->Word256->IO ()
mStore m@(Memory arr _) p val = do
  putStrLn "fourth"
  sequence_ $ uncurry (writeArray arr) <$> zip [p..] (word256ToBytes val)
  setNewMaxSize m (p+31)

mStore8::Memory->Word256->Word8->IO ()
mStore8 m@(Memory arr _) p val = do
  putStrLn "fifth"
  writeArray arr p val
  setNewMaxSize m p

mStoreByteString::Memory->Word256->B.ByteString->IO ()
mStoreByteString m@(Memory arr _) p theData = do
  putStrLn $ "sixth: " ++ show p ++ ", " ++ show (B.length theData)
  sequence_ $ uncurry (writeArray arr) <$> zip [p..p+fromIntegral (B.length theData)] (B.unpack theData)
  setNewMaxSize m (p + fromIntegral (B.length theData))

