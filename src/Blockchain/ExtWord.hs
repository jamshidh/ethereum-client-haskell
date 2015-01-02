{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.ExtWord (
  Word256
  ) where

import Network.Haskoin.Crypto (Word256)

import Data.Ix

instance Ix Word256 where
    range (x, y) | x == y = [x]
    range (x, y) = x:range (x+1, y)
    index (x, y) z | z < x || z > y = error $ "Ix{Word256}.index: Index (" ++ show z ++ ") out of range ((" ++ show x ++ "," ++ show y ++ "))"
    index (x, _) z = fromIntegral $ z - x
    inRange (x, y) z | z >= x && z <= y = True 
    inRange _ _ = False
