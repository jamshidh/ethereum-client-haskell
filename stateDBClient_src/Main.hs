{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Functor
import Data.List
import qualified Database.LevelDB as DB
import System.Environment
import Text.PrettyPrint.Leijen hiding ((<$>))

import qualified Data.NibbleString as N

import Database.DBs
import Database.MerklePatricia
import Format
import Data.RLP

main = do
  [theType, addr] <- getArgs
  let sr = SHAPtr $ fst $ B16.decode $ BC.pack addr
  let useCppDb =
        case theType of
          "h" -> False
          "c" -> True
  DB.runResourceT $ do
    db <- openDBs useCppDb --True = .ethereum, False = .ethereumH
    kvs <- getKeyVals db{stateRoot=sr} ""
    liftIO $ putStrLn $ intercalate "\n" ((\(k, v) -> format k ++ ": " ++ show (pretty $ rlpDeserialize $ rlpDecode v)) <$> filter (filterUnnecessary . fst) kvs)

filterUnnecessary::N.NibbleString->Bool
filterUnnecessary "1a26338f0d905e295fccb71fa9ea849ffa12aaf4" = False
filterUnnecessary "2ef47100e0787b915105fd5e3f4ff6752079d5cb" = False
filterUnnecessary "51ba59315b3a95761d0863b05ccc7a7f54703d99" = False
filterUnnecessary "6c386a4b26f73c802f34673f7248bb118f97424a" = False
filterUnnecessary "b9c015918bdaba24b4ff057a92a3873d6eb201be" = False
filterUnnecessary "cd2a3d9f938e13cd947ec05abc7fe734df8dd826" = False
filterUnnecessary "e4157b34ea9615cfbde6b4fda419828124b70c78" = False
filterUnnecessary "e6716f9544a56c530d868e4bfbacb172315bdead" = False
filterUnnecessary _ = True
