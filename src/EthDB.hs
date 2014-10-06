{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module EthDB (
  showAllKeyVal,
  SHAPtr(..),
  getKeyVals,
  putKeyVal,
  kvFormat,
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString.Internal
import qualified Data.ByteString.Char8 as BC
import Data.Default
import Data.Functor
import Data.List
import Database.LevelDB
import Numeric

import Colors
import Format
import qualified NibbleString as N
import RLP

--import Debug.Trace

showAllKeyVal::DB->Iterator->ResourceT IO ()
showAllKeyVal db i = do
  Just key <- iterKey i
  Just byteStringValue <- iterValue i
  let val = (rlpDecode $ rlpDeserialize $ byteStringValue)::NodeData
  liftIO $ putStrLn $ "----------\n" ++ format (SHAPtr key)
  liftIO $ putStrLn $ format val
  iterNext i
  v <- iterValid i
  if v
     then showAllKeyVal db i
     else return ()

getNodeData::DB->SHAPtr->ResourceT IO NodeData
getNodeData db (SHAPtr p) = do
  maybeBytes <- get db def p
  case maybeBytes of
    Just bytes -> return $ rlpDecode $ rlpDeserialize bytes
    Nothing -> error ("getNodeData node not found: " ++ format p)

getKeyVals::DB->SHAPtr->N.NibbleString->ResourceT IO [(N.NibbleString, B.ByteString)]
getKeyVals db p key = do
  nodeData <- getNodeData db p
  nextVals <- 
    case nodeData of
      FullNodeData {choices=cs} -> do
        if N.null key
          then concat <$> sequence [fmap (prependToKey (N.singleton nextN)) <$> getKeyVals db nextP ""| (nextN, Just nextP) <- zip [0..] cs]
          else case cs!!fromIntegral (N.head key) of
          Just nextP -> fmap (prependToKey $ N.singleton $ N.head key) <$> getKeyVals db nextP (N.tail key)
          Nothing -> return []
      ShortcutNodeData{nextNibbleString=s,nextVal=Right v} | key `N.isPrefixOf` s ->
        return [(s, v)]
      ShortcutNodeData{nextNibbleString=s,nextVal=Left nextP} | key `N.isPrefixOf` s ->
        fmap (prependToKey s) <$> getKeyVals db nextP ""
      ShortcutNodeData{nextNibbleString=s,nextVal=Left nextP} | s `N.isPrefixOf` key ->
        fmap (prependToKey s) <$> getKeyVals db nextP (N.drop (N.length s) key)
      _ -> return []
  case (N.null key, nodeData) of
    (True, FullNodeData{nodeVal = Just v}) -> return (("", v):nextVals)
    _ -> return nextVals

putKeyVal::DB->SHAPtr->N.NibbleString->B.ByteString->ResourceT IO SHAPtr
putKeyVal _ _ _ _ = error "putKeyVal not defined"


prependToKey::N.NibbleString->(N.NibbleString, B.ByteString)->(N.NibbleString, B.ByteString)
prependToKey prefix (key, val) = (prefix `N.append` key, val)

newtype SHAPtr = SHAPtr B.ByteString deriving (Show)

instance Format SHAPtr where
  format (SHAPtr x) = yellow $ format x

data NodeData = 
  FullNodeData {
    choices::[Maybe SHAPtr],
    nodeVal::Maybe B.ByteString
  } |
  ShortcutNodeData {
    nextNibbleString::N.NibbleString,
    nextVal::Either SHAPtr B.ByteString
  } deriving Show

formatVal::Maybe B.ByteString->String
formatVal Nothing = red "NULL"
formatVal (Just x) = green (format x)

instance Format NodeData where
  format (ShortcutNodeData s (Left p)) = "    " ++ format s ++ " -> " ++ format p
  format (ShortcutNodeData s (Right val)) = "    " ++ format s ++ " -> " ++ green (format val)
  format (FullNodeData cs val) = "    val: " ++ formatVal val ++ "\n        " ++ intercalate "\n        " (showChoice <$> zip ([0..]::[Int]) cs)
    where
      showChoice (v, Just p) = blue (showHex v "") ++ ": " ++ green (format p)
      showChoice (v, Nothing) = blue (showHex v "") ++ ": " ++ red "NULL"

string2TermNibbleString::String->(Bool, N.NibbleString)
string2TermNibbleString [] = error "string2TermNibbleString called with empty String"
string2TermNibbleString (c:rest) = --trace ("string2TermNibbleString: " ++ show (c2w c) ++ ": " ++ show flags) $
  (terminator, s)
  where
    w = c2w c
    (flags, extraNibble) = if w > 0xF then (w `shiftR` 4, 0xF .&. w) else (w, 0)
    terminator = flags `shiftR` 1 == 1
    oddLength = flags .&. 1 == 1
    s = if oddLength then N.OddNibbleString extraNibble (BC.pack rest) else N.EvenNibbleString (BC.pack rest)

instance RLPSerializable NodeData where
  rlpEncode = error("rlpEncode undefined")
  rlpDecode (RLPArray [a, RLPString val]) = --trace ("rlpDecode for NodeData: " ++ hexFormat (BC.pack val)) $
    if terminator
    then ShortcutNodeData s (Right $ BC.pack val)
    else ShortcutNodeData s (Left $ SHAPtr (BC.pack val))
    where
      (terminator, s) = string2TermNibbleString $ rlpDecode a
  rlpDecode (RLPArray x) | length x == 17 =
    FullNodeData (fmap getPtr <$> (\p -> case p of RLPNumber 0 -> Nothing; _ -> Just p) <$> childPointers) val
    where
      childPointers = init x
      val = case last x of
        RLPNumber 0 -> Nothing
        RLPString s -> Just $ BC.pack s
        _ -> error "Malformed RLP data in call to rlpDecode for NodeData: value of FullNodeData is a RLPArray"
      getPtr::RLPObject->SHAPtr
      getPtr p = SHAPtr $ rlpDecode p
  rlpDecode x = error ("Missing case in rlpDecode for NodeData: " ++ show x)

kvFormat::(N.NibbleString, B.ByteString)->String
kvFormat (k, v) = format k ++ ": " ++ format rlpVal
  where
    rlpVal = rlpDeserialize v

