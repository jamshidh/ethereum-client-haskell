{-# LANGUAGE OverloadedStrings #-}

module Data.AddressState (
  AddressState(..),
  blankAddressState,
  getAddressState,
  getAllAddressStates,
  putAddressState
  ) where

import qualified Data.ByteString as B
import Data.Functor
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Data.Address
import qualified Colors as CL
import Context
import ExtDBs
import Format
import qualified Data.NibbleString as N
import Data.RLP
import SHA
import Util

--import Debug.Trace

data AddressState = AddressState { addressStateNonce::Integer, balance::Integer, contractRoot::Maybe SHAPtr, codeHash::SHA } deriving (Show)


blankAddressState::AddressState
blankAddressState = AddressState { addressStateNonce=0, balance=0, contractRoot=Nothing, codeHash=hash B.empty }

instance Format AddressState where
  format a = CL.blue "AddressState" ++
             tab("\nnonce: " ++ showHex (addressStateNonce a) "" ++
                 "\nbalance: " ++ show (toInteger $ balance a) ++
                 "\ncontractRoot: " ++ formatCR (contractRoot a) ++
                 "\ncodeHash: " ++ show (pretty $ codeHash a))
    where
      formatCR::Maybe SHAPtr->String
      formatCR Nothing = "0x0"
      formatCR (Just x) = show $ pretty x
  
instance RLPSerializable AddressState where
  rlpEncode a = RLPArray [
    rlpEncode $ toInteger $ addressStateNonce a,
    rlpEncode $ toInteger $ balance a,
    case contractRoot a of
         Nothing -> rlpEncode (0::Integer)
         Just x -> rlpEncode x,
    rlpEncode $ codeHash a]

  rlpDecode (RLPArray [n, b, cr, ch]) =
    AddressState {
      addressStateNonce=fromInteger $ rlpDecode n,
      balance=fromInteger $ rlpDecode b,
      contractRoot=if rlpDecode cr == (0::Integer) then Nothing else Just (rlpDecode cr),
      codeHash=rlpDecode ch
      } 
  rlpDecode x = error $ "Missing case in rlpDecode for AddressState: " ++ show (pretty x)

addressAsNibbleString::Address->N.NibbleString
addressAsNibbleString (Address s) = N.EvenNibbleString $ B.pack $ integer2Bytes $ fromIntegral s

getAddressState::Address->ContextM AddressState
getAddressState address = do
  states <- getKeyVals $ addressAsNibbleString address
  case states of
    [] -> return blankAddressState
    [state] -> return $ rlpDecode $ rlpDeserialize $ rlpDecode $ snd state
    _ -> error ("getAddressStates found multiple states for: " ++ show (pretty address))
  

getAllAddressStates::ContextM [(N.NibbleString, AddressState)]
getAllAddressStates = do
  states <- getKeyVals ""
  return $ fmap rlpDecode <$> states

  

putAddressState::Address->AddressState->ContextM ()
putAddressState address newState = 
  putKeyVal (addressAsNibbleString address) $ rlpEncode $ rlpSerialize $ rlpEncode newState

