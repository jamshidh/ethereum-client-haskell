
module Memory (
  Memory,
  memLoad,
  memStore,
  memStore8
  ) where

import Data.Word

import ExtWord

data Memory = Memory

memLoad::Integer->Integer->IO [Word8]
memLoad p size = undefined

memStore::Integer->Word256->IO ()
memStore p val = undefined

memStore8::Integer->Word8->IO ()
memStore8 p val = undefined

