{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Blockchain.Format (
  Format(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.ExtWord

import Blockchain.Util

class Format a where
  format::a->String

instance Format B.ByteString where
  format x = BC.unpack (B16.encode x)

instance Pretty Word256 where
  pretty x = text $ BC.unpack (B16.encode $ B.pack $ word256ToBytes x)

