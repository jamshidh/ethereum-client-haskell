{-# OPTIONS_GHC -Wall #-}

module ExtendedECDSA (
  ExtendedSignature(..),
  extSignMsg,
  getPubKeyFromSignature 
  ) where

import Control.Monad
import qualified Control.Monad.State as S
import Control.Monad.Trans (lift)
import Data.Bits

import Network.Haskoin.Constants
import Network.Haskoin.Crypto
import Network.Haskoin.Internals
import Network.Haskoin.Util

nextSecret :: Monad m => SecretT m FieldN
nextSecret = do
    (ws,f) <- S.get
    let (ws',randM) = hmacDRBGGen ws 32 (stringToBS haskoinUserAgent)
    case randM of
        (Just rand) -> do
            S.put (ws',f)
            let randI = bsToInteger rand
            if isIntegerValidKey randI
                then return $ fromInteger randI
                else nextSecret
        Nothing -> do
            seed <- lift $ f 32 -- Read 256 bits to re-seed the PRNG
            let ws0 = hmacDRBGRsd ws' seed (stringToBS haskoinUserAgent)
            S.put (ws0,f)
            nextSecret

genKeyPair :: Monad m => SecretT m (FieldN, Point)
genKeyPair = do
    -- 3.2.1.1 
    d <- nextSecret
    -- 3.2.1.2
    let q = mulPoint d curveG
    -- 3.2.1.3
    return (d,q)

-----------------------

data ExtendedSignature = ExtendedSignature Signature Bool deriving (Show, Eq)

unsafeExtSignMsg :: Word256 -> FieldN -> (FieldN, Point) -> Maybe ExtendedSignature
unsafeExtSignMsg _ 0 _ = Nothing
unsafeExtSignMsg h d (k,p) = do
    -- 4.1.3.1 (4.1.3.2 not required)
    (x,y) <- getAffine p
    -- 4.1.3.3
    let r = (fromIntegral x :: FieldN)
    guard (r /= 0)
    -- 4.1.3.4 / 4.1.3.5
    let e = (fromIntegral h :: FieldN)
    -- 4.1.3.6
    let s' = (e + r*d)/k
        -- Canonicalize signatures: s <= order/2
        -- maxBound/2 = (maxBound+1)/2 = order/2 (because order is odd)
        s  = if s' > (maxBound `div` 2) then (-s') else s'
    guard (s /= 0)
    -- 4.1.3.7
    --return $ (Signature r s, odd y `xor` (s' > (maxBound `div` 2)))
    return $ ExtendedSignature (Signature r s) (odd y `xor` (s' > (maxBound `div` 2)))

extSignMsg :: Monad m => Word256 -> PrvKey -> SecretT m ExtendedSignature
extSignMsg _ (PrvKey  0) = error "signMsg: Invalid private key 0"
extSignMsg _ (PrvKeyU 0) = error "signMsg: Invalid private key 0"
extSignMsg h d = do
    -- 4.1.3.1
    (k,p) <- genKeyPair
    case unsafeExtSignMsg h (prvKeyFieldN d) (k,p) of
        (Just sig) -> return sig
        -- If signing failed, retry with a new nonce
        Nothing    -> extSignMsg h d

-------------------

getPubKeyFromSignature::ExtendedSignature->Word256->PubKey
getPubKeyFromSignature (ExtendedSignature sig yIsOdd) msgHash = 
  PubKey $ ((s / r) `mulPoint` bigR) `addPoint` ((fromIntegral curveN - fromIntegral msgHash/r) `mulPoint` curveG)
  where
    r = sigR sig
    s = sigS sig
    ys = quadraticResidue $ (fromIntegral r)^(3::Integer)+7
    correctY = if odd (ys !! 0) == yIsOdd then ys !! 0 else ys !! 1
    Just bigR = makePoint (fromIntegral r) correctY
