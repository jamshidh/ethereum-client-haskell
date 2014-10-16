
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

prop_split::[Int]->Bool
prop_split [] = True
prop_split (10:rest) = False
prop_split x = head x:tail x == x

showKVs (k, v) = format k ++ ": " ++ format v

verifyRoundTrip::String->SHAPtr->IO Bool
verifyRoundTrip dbPath stateRoot = do
  runResourceT $ do
    db1 <- open dbPath def

    kvs <- getKeyVals db1 stateRoot N.empty

    liftIO $ putStrLn $ "kvs: " ++ concat (showKVs <$> kvs)

    (db2, startingStateRoot) <- initializeBlankStateDB "/tmp/tmpDb2"

    stateRoot2 <- putKeyVals db2 startingStateRoot kvs

    kvs2 <- getKeyVals db2 stateRoot2 N.empty
    liftIO $ putStrLn $ "kvs2: " ++ concat (showKVs <$> kvs2)

    liftIO $ putStrLn $ "stateRoot = " ++ format stateRoot
    liftIO $ putStrLn $ "stateRoot2 = " ++ format stateRoot2

    return $ stateRoot == stateRoot2

putKeyVals::DB->SHAPtr->[(N.NibbleString, B.ByteString)]->ResourceT IO SHAPtr
putKeyVals db stateRoot [(k,v)] = putKeyVal db stateRoot k v
putKeyVals db stateRoot ((k, v):rest) = do
  newStateRoot <- putKeyVal db stateRoot k v
  putKeyVals db newStateRoot rest

prop_testStateDB::Assertion
prop_testStateDB = do
  stateRoot2 <- runResourceT $ do
    (db, stateRoot) <- initializeBlankStateDB "/tmp/tmpDb1"
    putKeyVals db stateRoot
      [
        (N.EvenNibbleString $ BC.pack "abcd", BC.pack "abcd"),
        (N.EvenNibbleString $ BC.pack "aefg", BC.pack "aefg")
      ]

  result <- verifyRoundTrip "/tmp/tmpDb1" stateRoot2

  assertBool "empty db didn't match" result


main::IO ()
main = 
  defaultMainWithOpts [testCase "qqqq" prop_testStateDB] mempty
    
{-
  defaultMain
  [
    testGroup "stateDB Tests"
    [
      testProperty "qqqq" prop_testStateDB
    ]
  ]
-}
