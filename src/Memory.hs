{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Memory (
  Memory,
  newMemory,
  mLoad,
  mLoad8,
  mStore,
  mStore8
  ) where

import Data.Array.IO
import Data.Functor
import Data.Ix
import Data.Word

import ExtWord
import Util

data Memory = Memory Word256 (IOArray Word256 Word8)


instance Ix Word256 where
    range (x, y) | x == y = [x]
    range (x, y) = x:range (x+1, y)
    index (x, y) z | z < x || z > y = error $ "Ix{Word256}.index: Index (" ++ show z ++ ") out of range ((" ++ show x ++ "," ++ show y ++ "))"
    index (x, _) z = fromIntegral $ z - x
    inRange (x, y) z | z >= x && z <= y = True 
    inRange _ _ = False


newMemory::IO Memory
newMemory = do
  m <- newArray (0, 100) 0
  return $ Memory 0 m

mLoad::Memory->Word256->IO [Word8]
mLoad (Memory _ arr) p = sequence $ readArray arr <$> [p..p+31] 

mLoad8::Memory->Word256->IO Word8
mLoad8 (Memory _ arr) p = readArray arr p

mStore::Memory->Word256->Word256->IO ()
mStore (Memory _ arr) p val = sequence_ $ uncurry (writeArray arr) <$> zip [p..] (word256ToBytes val)

mStore8::Memory->Word256->Word8->IO ()
mStore8 m p val = undefined

