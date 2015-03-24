{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Blockchain.VM.Memory (
  Memory(..),
  getSizeInBytes,
  getSizeInWords,
  getShow,
  getMemAsByteString,
  mLoad,
  mLoad8,
  mLoadByteString,
  unsafeSliceByteString,
  mStore,
  mStore8,
  mStoreByteString
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.State hiding (state)
import qualified Data.Vector.Storable.Mutable as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Functor
import Data.IORef
import Data.Word
import Foreign
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Blockchain.Colors as CL
import Blockchain.ExtWord
import Blockchain.Util
import Blockchain.VM.OpcodePrices
import Blockchain.VM.VMState
import Blockchain.VM.VMM

safeRead::V.IOVector Word8->Word256->IO Word8
safeRead _ x | x > 0x7fffffffffffffff = return 0 --There is no way that memory will be filled up this high, it would cost too much gas.  I think it is safe to assume it is zero.
--safeRead _ x | x > 0x7fffffffffffffff = error "error in safeRead, value too large"
safeRead mem x = do
  let len = V.length mem
  if x < fromIntegral len
    then V.read mem (fromIntegral x)
    else return 0

--Word256 is too big to use for [first..first+size-1], so use safeRange instead
safeRange::Word256->Word256->[Word256]
safeRange _ 0 = []
safeRange first size = first:safeRange (first+1) (size-1)

getSizeInWords::VMM Word256
getSizeInWords = do
  state <- lift get
  let (Memory _ size) = memory state
  liftIO $ (ceiling . (/ (32::Double)) . fromIntegral) <$> readIORef size

getSizeInBytes::VMM Word256
getSizeInBytes = do
  state <- lift get
  let (Memory _ size) = memory state
  liftIO $ fromIntegral <$> readIORef size

--In this function I use the words "size" and "length" to mean 2 different things....
--"size" is the highest memory location used (as described in the yellowpaper).
--"length" is the IOVector length, which will often be larger than the size.
--Basically, to avoid resizing the vector constantly (which could be expensive),
--I keep something larger around until it fills up, then reallocate (something even
--larger).
setNewMaxSize::Integer->VMM ()
setNewMaxSize newSize' = do
  --TODO- I should just store the number of words....  memory size can only be a multiple of words.
  --For now I will just use this hack to allocate to the nearest higher number of words.
  let newSize = 32 * ceiling (fromIntegral newSize'/(32::Double))::Integer
  state <- lift get

  oldSize <- liftIO $ readIORef (mSize $ memory state)


  let gasCharge =
        if newSize > fromIntegral oldSize
        then
          let newWordSize = fromInteger $ (ceiling $ fromIntegral newSize/(32::Double))
              oldWordSize = (ceiling $ fromIntegral oldSize/(32::Double))
              sizeCost c = gMEMWORD * c + (c*c `quot` gQUADCOEFFDIV)
          in sizeCost newWordSize - sizeCost oldWordSize
          else 0

  let oldLength = fromIntegral $ V.length (mVector $ memory state)

  if vmGasRemaining state < gasCharge
     then do
          setGasRemaining 0
          left OutOfGasException
    else do
    when (newSize > fromIntegral oldSize) $ do
      liftIO $ writeIORef (mSize $ memory state) (fromInteger newSize)
    if newSize > oldLength
      then do
        state' <- lift get
        when (newSize > 100000000) $ liftIO $ putStrLn $ CL.red ("Warning, memory needs to grow to a huge value: " ++ show (fromIntegral newSize/1000000) ++ "MB")
        arr' <- liftIO $ V.grow (mVector $ memory state') $ fromIntegral $ (newSize+1000000)
        when (newSize > 100000000) $ liftIO $ putStrLn $ CL.red $ "clearing out memory"
        --liftIO $ forM_ [oldLength..(newSize+1000000)-1] $ \p -> V.write arr' (fromIntegral p) 0
        liftIO $ V.set (V.unsafeSlice (fromIntegral oldLength) (fromIntegral newSize+1000000) arr') 0
        when (newSize > 100000000) $ liftIO $ putStrLn $ CL.red $ "Finished growing memory"
        lift $ put $ state'{memory=(memory state'){mVector = arr'}}
      else return ()

    useGas gasCharge

getShow::Memory->IO String
getShow (Memory arr sizeRef) = do
  msize <- readIORef sizeRef
  --fmap (show . B16.encode . B.pack) $ sequence $ V.read arr <$> fromIntegral <$> [0..fromIntegral msize-1] 
  fmap (show . B16.encode . B.pack) $ sequence $ safeRead arr <$> [0..fromIntegral msize-1] 

getMemAsByteString::Memory->IO B.ByteString
getMemAsByteString (Memory arr sizeRef) = do
  msize <- readIORef sizeRef
  liftIO $ fmap B.pack $ sequence $ safeRead arr <$> [0..fromIntegral msize-1] 

mLoad::Word256->VMM [Word8]
mLoad p = do
  setNewMaxSize (fromIntegral p+32)
  state <- lift get
  liftIO $ sequence $ safeRead (mVector $ memory state) <$> safeRange p 32

mLoad8::Word256->VMM Word8
mLoad8 p = do
  --setNewMaxSize m p
  state <- lift get
  liftIO $ safeRead (mVector $ memory state) (fromIntegral p)

mLoadByteString::Word256->Word256->VMM B.ByteString
mLoadByteString _ 0 = return B.empty --no need to charge gas for mem change if nothing returned
mLoadByteString p size = do
  setNewMaxSize (fromIntegral p+fromIntegral size)
  state <- lift get
  val <- liftIO $ fmap B.pack $ sequence $ safeRead (mVector $ memory state) <$> fromIntegral <$> safeRange p size 
  return val

unsafeSliceByteString::Word256->Word256->VMM B.ByteString
unsafeSliceByteString p size = do
  setNewMaxSize (fromIntegral p+fromIntegral size)
  state <- lift get
  let (fptr, len) = V.unsafeToForeignPtr0 (V.slice (fromIntegral p) (fromIntegral size) $ mVector $ memory state)
  liftIO $ withForeignPtr fptr $ \ptr ->
    B.packCStringLen (castPtr ptr, len * sizeOf (undefined :: Word8))
    

mStore::Word256->Word256->VMM ()
mStore p val = do
  setNewMaxSize (fromIntegral p+32)
  state <- lift get
  liftIO $ sequence_ $ uncurry (V.write $ mVector $ memory state) <$> zip [fromIntegral p..] (word256ToBytes val)

mStore8::Word256->Word8->VMM ()
mStore8 p val = do
  setNewMaxSize (fromIntegral p+1)
  state <- lift get
  liftIO $ V.write (mVector $ memory state) (fromIntegral p) val

mStoreByteString::Word256->B.ByteString->VMM ()
mStoreByteString p theData = do
  setNewMaxSize (fromIntegral p + fromIntegral (B.length theData))
  state <- lift get
  liftIO $ sequence_ $ uncurry (V.write $ mVector $ memory state) <$> zip (fromIntegral <$> safeRange p (fromIntegral $ B.length theData)) (B.unpack theData)

