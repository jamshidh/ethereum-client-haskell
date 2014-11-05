{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Functor
import Data.List
import qualified Database.LevelDB as DB
import System.Environment

import DBs
import EthDB
import Format
import RLP

main = do
  [theType, addr] <- getArgs
  let stateRoot = SHAPtr $ fst $ B16.decode $ BC.pack addr
  let useCppDb =
        case theType of
          "h" -> False
          "c" -> True
  DB.runResourceT $ do
    db <- openDBs useCppDb --True = .ethereum, False = .ethereumH
    kvs <- getKeyVals db stateRoot ""
    liftIO $ putStrLn $ intercalate "\n" ((\(k, v) -> format k ++ ": " ++ format (rlpDeserialize v)) <$> kvs)
