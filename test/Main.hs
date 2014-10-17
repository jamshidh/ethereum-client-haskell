
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Default
import Data.Functor
import Data.List
import Data.Monoid
import qualified Data.Set as S
import Database.LevelDB
import System.Exit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Address
import EthDB
import Format
import ModifyStateDB
import qualified NibbleString as N
import RLP
import SHA
import Util

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
    --return (db, stateRoot2)
    valuesOut <- getKeyVals db stateRoot2 (N.EvenNibbleString B.empty)
    liftIO $ assertEqual "empty db didn't match" (S.fromList valuesIn) (S.fromList valuesOut)
    return ()

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

addFirstReward = do
  stateRoot <- initializeStateDB

  stateRoot2 <- addReward stateRoot $ Address 0x6ccf6b5c33ae2017a6c76b8791ca61276a69ab8e

  putStrLn $ format stateRoot2

  runResourceT $ do
    db <- open "/home/jim/.ethereumH/state" defaultOptions
    kvs <- getKeyVals db stateRoot2 N.empty
    liftIO $ putStrLn $ intercalate "\n" ((\(k, v) -> format k ++ ": " ++ format (rlpDeserialize v)) <$> kvs)

  putStrLn "-----------------------------"

  runResourceT $ do
    db <- open "/home/jim/.ethereum/state" defaultOptions
    kvs <- getKeyVals db (SHAPtr $ B.pack $ integer2Bytes 0x8b9a53acdafe82b6247c195035995f892f467fda03d7b44b684dcb3663fb3497) N.empty
    --kvs <- getKeyVals db (SHAPtr $ B.pack $ integer2Bytes 0x8dbd704eb38d1c2b73ee4788715ea5828a030650829703f077729b2b613dd206) N.empty
    liftIO $ putStrLn $ intercalate "\n" ((\(k, v) -> format k ++ ": " ++ format (rlpDeserialize v)) <$> kvs)

  assertEqual "reward state root doesn't match" stateRoot2 (SHAPtr $ B.pack $ integer2Bytes 0x8b9a53acdafe82b6247c195035995f892f467fda03d7b44b684dcb3663fb3497)


main::IO ()
main = 
  defaultMainWithOpts 
  [
   testCase "ShortcutNodeData Insert" testShortcutNodeDataInsert,
   testCase "FullNodeData Insert" testFullNodeDataInsert,
   testCase "FullNodeData Insert" addFirstReward
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
