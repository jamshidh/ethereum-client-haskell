{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module EthDB (
  showAllKeyVal,
  SHAPtr(..),
  getKeyVals,
  putKeyVal,
  kvFormat
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Crypto.Hash.SHA3 as C
import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString.Internal
import qualified Data.ByteString.Char8 as BC
import Data.Default
import Data.Function
import Data.Functor
import Data.List
import Database.LevelDB
import Numeric

import Address
import Colors
import Format
import qualified NibbleString as N
import Util
import RLP

--import Debug.Trace

showAllKeyVal::DB->ResourceT IO ()
showAllKeyVal db = do
  i <- iterOpen db def
  iterFirst i
  valid <- iterValid i
  if valid
    then showAllKeyVal' db i
    else liftIO $ putStrLn "no keys"
  where
    showAllKeyVal'::DB->Iterator->ResourceT IO ()
    showAllKeyVal' db i = do
      Just key <- iterKey i
      val <- getNodeData db (SHAPtr key)
      --Just byteStringValue <- iterValue i
      --let val = (rlpDecode $ rlpDeserialize $ byteStringValue)::NodeData
      liftIO $ putStrLn $ "----------\n" ++ format (SHAPtr key)
      liftIO $ putStrLn $ format val
      iterNext i
      v <- iterValid i
      if v
        then showAllKeyVal' db i
        else return ()

getNodeData::DB->SHAPtr->ResourceT IO NodeData
getNodeData db (SHAPtr p) = do
  maybeBytes <- get db def p
  case maybeBytes of
    Just bytes | B.null bytes -> return EmptyNodeData
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

nodeDataSerialize::NodeData->B.ByteString
nodeDataSerialize EmptyNodeData = B.empty
nodeDataSerialize x = rlpSerialize $ rlpEncode x

putNodeData::DB->NodeData->ResourceT IO SHAPtr
putNodeData db nd = do
  let bytes = nodeDataSerialize nd
      ptr = C.hash 256 bytes
  put db def ptr bytes
  return $ SHAPtr ptr

slotIsEmpty::[Maybe SHAPtr]->N.Nibble->Bool
slotIsEmpty [] _ = error ("slotIsEmpty was called for value greater than the size of the list")
slotIsEmpty (Nothing:_) 0 = True
slotIsEmpty _ 0 = False
slotIsEmpty (_:rest) n = slotIsEmpty rest (n-1)

replace::Integral i=>[a]->i->a->[a]
replace list i newVal = left ++ [newVal] ++ right
            where
              (left, _:right) = splitAt (fromIntegral i) list

getNewNodeDataFromPut::DB->N.NibbleString->B.ByteString->NodeData->ResourceT IO NodeData
getNewNodeDataFromPut _ key val EmptyNodeData = return $
  ShortcutNodeData key $ Right val
getNewNodeDataFromPut db key val (FullNodeData options nodeVal)
  | options `slotIsEmpty` N.head key = do
  tailNode <- putNodeData db (ShortcutNodeData (N.tail key) $ Right val)
  return $ FullNodeData (replace options (N.head key) $ Just tailNode) nodeVal

getNewNodeDataFromPut db key val (FullNodeData options nodeVal) = do
  let Just conflictingNode = options!!fromIntegral (N.head key)
  conflictingNodeData <- getNodeData db conflictingNode
  newNodeData <- getNewNodeDataFromPut db (N.tail key) val conflictingNodeData
  newNode <- putNodeData db newNodeData
  return $ FullNodeData (replace options (N.head key) $ Just newNode) nodeVal

getNewNodeDataFromPut _ key1 val (ShortcutNodeData key2 _) | key1 == key2 = 
  return $ ShortcutNodeData key1 $ Right val
getNewNodeDataFromPut _ key1 val (ShortcutNodeData key2 _) | N.head key1 == N.head key2 = 
  return $ ShortcutNodeData key1 $ Right val
getNewNodeDataFromPut db key1 val1 (ShortcutNodeData key2 val2) = do
  tailNode1 <- putNodeData db (ShortcutNodeData (N.tail key1) $ Right val1)
  tailNode2 <- putNodeData db (ShortcutNodeData (N.tail key2) val2)
  return $ FullNodeData
      (list2Options 0 (sortBy (compare `on` fst) [(N.head key1, tailNode1), (N.head key2, tailNode2)]))
      Nothing
  where
    list2Options::N.Nibble->[(N.Nibble, SHAPtr)]->[Maybe SHAPtr]
    list2Options start [] = replicate (fromIntegral $ 0x10 - start) Nothing
    list2Options start ((firstNibble, firstPtr):rest) =
      replicate (fromIntegral $ firstNibble - start) Nothing ++ [Just firstPtr] ++ list2Options (firstNibble+1) rest
getNewNodeDataFromPut _ key _ nd = error ("Missing case in getNewNodeDataFromPut: " ++ format nd ++ ", " ++ format key)

putKeyVal::DB->SHAPtr->N.NibbleString->B.ByteString->ResourceT IO SHAPtr
putKeyVal db p key val = do
  curNodeData <- getNodeData db p
  nextNodeData <- getNewNodeDataFromPut db key val curNodeData
  let key = C.hash 256 $ nodeDataSerialize nextNodeData 
  put db def key $ nodeDataSerialize nextNodeData
  return $ SHAPtr key

prependToKey::N.NibbleString->(N.NibbleString, B.ByteString)->(N.NibbleString, B.ByteString)
prependToKey prefix (key, val) = (prefix `N.append` key, val)

newtype SHAPtr = SHAPtr B.ByteString deriving (Show)

instance Format SHAPtr where
  format (SHAPtr x) = yellow $ format x

data NodeData =
  EmptyNodeData |
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
  format EmptyNodeData = "    <EMPTY>"
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

termNibbleString2String::Bool->N.NibbleString->B.ByteString
termNibbleString2String terminator s =
  case s of
    (N.EvenNibbleString s') -> B.singleton (extraNibble `shiftL` 4) `B.append` s'
    (N.OddNibbleString n rest) -> B.singleton (extraNibble `shiftL` 4 + N.head s) `B.append` rest
  where
    nibbleString2String::N.NibbleString->String
    nibbleString2String (N.OddNibbleString c s) = w2c c:BC.unpack s
    nibbleString2String (N.EvenNibbleString s) = BC.unpack s
    extraNibble =
        (if terminator then 2 else 0) +
        (if oddLength then 1 else 0)
    oddLength = odd $ N.length s

instance RLPSerializable NodeData where
  rlpEncode EmptyNodeData = error "rlpEncode should never be called on EmptyNodeData.  Use rlpSerialize instead."
  rlpEncode (FullNodeData {choices=cs, nodeVal=val}) = RLPArray ((encodeChoice <$> cs) ++ [encodeVal val])
    where
      encodeChoice Nothing = RLPNumber 0
      encodeChoice (Just (SHAPtr x)) = rlpEncode x
      encodeVal Nothing = RLPNumber 0
      encodeVal (Just x) = rlpEncode x
  rlpEncode (ShortcutNodeData {nextNibbleString=s, nextVal=val}) =
    RLPArray[RLPString $ BC.unpack $ termNibbleString2String terminator s, encodeVal val] 
    where
      flags =
        (if terminator then 2 else 0) +
        (if oddLength then 1 else 0)
      terminator = 
        case val of
          Left _ -> False
          Right _ -> True
      oddLength = odd $ N.length s
      encodeVal (Left (SHAPtr x)) = rlpEncode x
      encodeVal (Right x) = rlpEncode x



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
  rlpDecode x = error ("Missing case in rlpDecode for NodeData: " ++ format x)

kvFormat::(N.NibbleString, B.ByteString)->String
kvFormat (k, v) = format k ++ ": " ++ format rlpVal
  where
    rlpVal = rlpDeserialize v

