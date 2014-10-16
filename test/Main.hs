
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Default
import Data.Functor
import Data.Monoid
import Database.LevelDB
import System.Exit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import EthDB
import Format
import ModifyStateDB
import qualified NibbleString as N
import SHA

putKeyVals::DB->SHAPtr->[(N.NibbleString, B.ByteString)]->ResourceT IO SHAPtr
putKeyVals db stateRoot [(k,v)] = putKeyVal db stateRoot k v
putKeyVals db stateRoot ((k, v):rest) = do
  newStateRoot <- putKeyVal db stateRoot k v
  putKeyVals db newStateRoot rest

verifyDBDataIntegrity::[(N.NibbleString, B.ByteString)]->IO ()
verifyDBDataIntegrity valuesIn = do
  runResourceT $ do
    (db, stateRoot) <- initializeBlankStateDB "/tmp/tmpDb1"
    stateRoot2 <- putKeyVals db stateRoot valuesIn
    return (db, stateRoot2)
    valuesOut <- getKeyVals db stateRoot2 (N.EvenNibbleString B.empty)
    liftIO $ assertEqual "empty db didn't match" valuesIn valuesOut

testShortcutNodeDataInsert::Assertion
testShortcutNodeDataInsert = do
  verifyDBDataIntegrity
        [
          (N.EvenNibbleString $ BC.pack "abcd", BC.pack "abcd"),
          (N.EvenNibbleString $ BC.pack "aefg", BC.pack "aefg")
        ]

testFullNodeDataInsert = do
  verifyDBDataIntegrity
        [
          (N.EvenNibbleString $ BC.pack "abcd", BC.pack "abcd"),
          (N.EvenNibbleString $ BC.pack "bb", BC.pack "bb"),
          (N.EvenNibbleString $ BC.pack "aefg", BC.pack "aefg")
        ]


main::IO ()
main = 
  defaultMainWithOpts 
  [
   testCase "ShortcutNodeData Insert" testShortcutNodeDataInsert,
   testCase "FullNodeData Insert" testFullNodeDataInsert
  ] mempty
    
{-
  defaultMain
  [
    testGroup "stateDB Tests"
    [
      testProperty "qqqq" prop_testStateDB
    ]
  ]
-}
