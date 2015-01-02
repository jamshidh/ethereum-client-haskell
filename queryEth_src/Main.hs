{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import System.Console.CmdArgs

import State
import Block
import Init
import Code

--import Debug.Trace



import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.ExtDBs


data Options = 
  State{root::String, db::String} 
  | Block{hash::String, db::String} 
  | Code{hash::String, db::String}
  | Init{hash::String, db::String}
  deriving (Show, Data, Typeable)

stateOptions::Annotate Ann
stateOptions = 
  record State{root=undefined, db=undefined} [
    root := def += typ "SHAPtr" += argPos 1,
    db := def += typ "DBSTRING" += argPos 0
    ]

blockOptions::Annotate Ann
blockOptions = 
  record Block{hash=undefined, db=undefined} [
    hash := def += typ "FILENAME" += argPos 1 += opt ("-"::String),
    db := def += typ "DBSTRING" += argPos 0
    ]

initOptions::Annotate Ann
initOptions = 
  record Init{hash=undefined, db=undefined} [
    hash := def += typ "FILENAME" += argPos 1 += opt ("-"::String),
    db := def += typ "DBSTRING" += argPos 0
    ]

codeOptions::Annotate Ann
codeOptions = 
  record Code{hash=undefined, db=undefined} [
    hash := def += typ "USERAGENT" += argPos 1,
    db := def += typ "DBSTRING" += argPos 0
    ]

options::Annotate Ann
options = modes_ [stateOptions, blockOptions, initOptions, codeOptions]


--      += summary "Apply shims, reorganize, and generate to the input"


main::IO ()
main = do
  opts <- cmdArgs_ options
  run opts
    
-------------------


run::Options->IO ()

run State{root=r, db=db'} = do
  let sr = SHAPtr $ fst $ B16.decode $ BC.pack r
  State.doit db' sr

run Block{hash=h, db=db'} = do
  Block.doit db' h

run Init{hash=h, db=db'} = do
  Init.doit db' h

run Code{hash=h, db=db'} = do
  Code.doit db' h

