{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
module NibbleString (
  Nibble,
  NibbleString(..),
  empty,
  singleton,
  null,
  length,
  pack,
  unpack,
  isPrefixOf,
  head,
  tail,
  drop,
  append
  ) where

import Prelude hiding (head, tail, length, drop, null)
import qualified Prelude

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.String
import Data.Word
import Numeric

import ByteStringTools()
import Colors
import Format

--import Debug.Trace

type Nibble = Word8

data NibbleString = EvenNibbleString B.ByteString | OddNibbleString Nibble B.ByteString deriving (Show, Eq)

formatNibble::Nibble->String
formatNibble x | x > 0xF = error "format called for nibble greater than 0xF"
formatNibble x = showHex x ""

instance Format NibbleString where
  format (EvenNibbleString s) = blue $ format s
  format (OddNibbleString c s) = blue $ formatNibble c ++ format s

instance IsString NibbleString where
  fromString "" = EvenNibbleString B.empty
  fromString s | even $ Prelude.length s = case B16.decode $ BC.pack s of
    (x, unparsable) | B.null unparsable -> EvenNibbleString x
    _ -> error ("fromString conversion to NibbleString failed.  The string was of the wrong format: " ++ show s)
  fromString (c:rest) = case B16.decode $ BC.pack rest of
    (x, unparsable) | B.null unparsable -> 
        case readHex [c] of
	  [(w, "")] -> OddNibbleString w x
    	  _ -> error ("fromString conversion to NibbleString failed.  The string was of the wrong format: " ++ show (c:rest))
    _ -> error ("fromString conversion to NibbleString failed.  The string was of the wrong format: " ++ show (c:rest))

length::NibbleString->Int
length (EvenNibbleString s) = B.length s `shiftL` 1
length (OddNibbleString _ s) = 1 + B.length s `shiftL` 1

singleton::Nibble->NibbleString
singleton c = OddNibbleString c B.empty

null::NibbleString->Bool
null (EvenNibbleString s) = B.null s
null (OddNibbleString _ _) = False

empty::NibbleString
empty = EvenNibbleString $ B.empty

append::NibbleString->NibbleString->NibbleString
append (EvenNibbleString s1) (EvenNibbleString s2) = EvenNibbleString (s1 `B.append` s2) 
append (OddNibbleString c1 s1) (EvenNibbleString s2) = OddNibbleString c1 (s1 `B.append` s2) 
append (OddNibbleString c1 s1) (OddNibbleString c2 s2) | B.null s1 = EvenNibbleString (B.cons (c1 `shiftL` 4 + c2) $ s1 `B.append` s2) 
--append (EvenNibbleString s1) (OddNibbleString c2 s2) = OddNibbleString (
--append (OddNibbleString c1 s1) (OddNibbleString c2 s2) = EvenNibbleString (s1 `B.append` s2) 
append x y = pack (unpack x ++ unpack y)
append x y = error ("Not implemented yet for append: " ++ show x ++ ", " ++ show y)

head::NibbleString->Nibble
--head n | trace ("calling head: (" ++ show (length n) ++ ")" ++ show n) $ False = undefined
head (OddNibbleString c _) = c 
head (EvenNibbleString s) = B.head s `shiftR` 4 

tail::NibbleString->NibbleString
--tail n | trace ("calling tail: " ++ show n) $ False = undefined
tail (OddNibbleString _ s) = EvenNibbleString s 
tail (EvenNibbleString s) = OddNibbleString (B.head s .&. 0xF) $ B.tail s

pack::[Nibble]->NibbleString
pack (c:rest) | even $ Prelude.length rest = c `prependNibble` pack rest
              where
                prependNibble c (EvenNibbleString x) = OddNibbleString c x
pack x = EvenNibbleString $ B.pack (nibbles2Bytes x)
    where
      nibbles2Bytes::[Nibble]->[Word8]
      nibbles2Bytes [] = []
      nibbles2Bytes (x:y:rest) = x `shiftL` 4 + y:nibbles2Bytes rest

unpack::NibbleString->[Nibble]
unpack (OddNibbleString c rest) = c:unpack (EvenNibbleString rest)
unpack (EvenNibbleString x) = byte2Nibbles =<< B.unpack x
    where
      byte2Nibbles x = [x `shiftR` 4, x .&. 0xF]


isPrefixOf::NibbleString->NibbleString->Bool
--isPrefixOf a b | trace ("isPrefixOf: " ++ format a ++ ", " ++ format b) False = undefined
isPrefixOf (EvenNibbleString s1) _ | B.null s1 = True
isPrefixOf (EvenNibbleString s1) (EvenNibbleString s2) = s1 `B.isPrefixOf` s2 
isPrefixOf (OddNibbleString c1 s1) n2 = 
  case length n2 of
    0 -> False
    _ -> c1 == head n2 && EvenNibbleString s1 `isPrefixOf` tail n2
isPrefixOf n1 n2 | head n1 == head n2 = tail n1 `isPrefixOf` tail n2
isPrefixOf x y = error ("Missing case in isPrefixOf: " ++ show x ++ ", " ++ show y)

drop::Int->NibbleString->NibbleString
drop 0 s = s
--drop n s | trace ("drop: " ++ show n ++ ", " ++ show s) False = undefined
drop n s | n > length s = empty
drop n (EvenNibbleString s) | even n = EvenNibbleString (B.drop (n `shiftR` 1) s)
drop 1 s = tail s
drop n (EvenNibbleString s) = drop 1 $ EvenNibbleString (B.drop ((n - 1) `shiftR` 1) s)
drop n (OddNibbleString _ s) | even n = drop (n-1) $ EvenNibbleString s
drop n (OddNibbleString _ s) = drop (n - 1) $ EvenNibbleString s
