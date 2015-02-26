{-# LANGUAGE OverloadedStrings #-}

module State 
    (
     doit
    ) where

import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Functor
import Data.List
import qualified Database.LevelDB as DB
import System.Environment
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Data.NibbleString as N
import Blockchain.Data.RLP

import Blockchain.Context
import Blockchain.ExtDBs
import Blockchain.Format

formatKV::(N.NibbleString, RLPObject)->Doc
formatKV (key, val) =
    pretty key <> text ": " <> pretty (rlpDeserialize $ rlpDecode val)

showVals::SHAPtr->ContextM ()
showVals sr = do
    setStateRoot sr
    kvs <- getKeyVals ""
    --liftIO $ putStrLn $ displayS (renderPretty 1.0 200 $ vsep $ formatKV <$> kvs) ""
    liftIO $ putStrLn $ displayS (renderPretty 1.0 200 $ vsep $ formatKV <$> filter (filterUnnecessary . fst) kvs) ""

doit::String->SHAPtr->IO()
doit theType sr = do
  DB.runResourceT $ do
    cxt <- openDBs theType
    _ <- liftIO $ runStateT (showVals sr) cxt
    return ()

filterUnnecessary::N.NibbleString->Bool
filterUnnecessary "1a26338f0d905e295fccb71fa9ea849ffa12aaf4" = False
filterUnnecessary "2ef47100e0787b915105fd5e3f4ff6752079d5cb" = False
filterUnnecessary "6c386a4b26f73c802f34673f7248bb118f97424a" = False
filterUnnecessary "b9c015918bdaba24b4ff057a92a3873d6eb201be" = False
filterUnnecessary "cd2a3d9f938e13cd947ec05abc7fe734df8dd826" = False
filterUnnecessary "e4157b34ea9615cfbde6b4fda419828124b70c78" = False
filterUnnecessary "e6716f9544a56c530d868e4bfbacb172315bdead" = False

filterUnnecessary "dbdbdb2cbd23b783741e8d7fcf51e459b497e4a6" = False
filterUnnecessary "b0afc46d9ce366d06ab4952ca27db1d9557ae9fd" = False
filterUnnecessary "f6b1e9dc460d4d62cc22ec5f987d726929c0f9f0" = False
filterUnnecessary "cc45122d8b7fa0b1eaa6b29e0fb561422a9239d0" = False
filterUnnecessary "b7576e9d314df41ec5506494293afb1bd5d3f65d" = False

filterUnnecessary _ = True






                      
