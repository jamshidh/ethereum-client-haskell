{-# LANGUAGE OverloadedStrings #-}

module Data.AddressState (
  AddressState(..),
  blankAddressState,
  getAddressState,
  getAllAddressStates,
  putAddressState
  ) where

import Data.Binary
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
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

data AddressState = AddressState { addressStateNonce::Integer, balance::Integer, contractRoot::SHAPtr, codeHash::SHA } deriving (Show)


blankAddressState::AddressState
blankAddressState = AddressState { addressStateNonce=0, balance=0, contractRoot=emptyTriePtr, codeHash=hash "" }

instance Format AddressState where
  format a = CL.blue "AddressState" ++
             tab("\nnonce: " ++ showHex (addressStateNonce a) "" ++
                 "\nbalance: " ++ show (toInteger $ balance a) ++
                 "\ncontractRoot: " ++ show (pretty $ contractRoot a) ++
                 "\ncodeHash: " ++ show (pretty $ codeHash a))
  
instance RLPSerializable AddressState where
  rlpEncode a | balance a < 0 = error $ "Error in cal to rlpEncode for AddressState: AddressState has negative balance: " ++ format a
  rlpEncode a = RLPArray [
    rlpEncode $ toInteger $ addressStateNonce a,
    rlpEncode $ toInteger $ balance a,
    rlpEncode $ contractRoot a,
    rlpEncode $ codeHash a
                ]

  rlpDecode (RLPArray [n, b, cr, ch]) =
    AddressState {
      addressStateNonce=fromInteger $ rlpDecode n,
      balance=fromInteger $ rlpDecode b,
      contractRoot=rlpDecode cr,
      codeHash=rlpDecode ch
      } 
  rlpDecode x = error $ "Missing case in rlpDecode for AddressState: " ++ show (pretty x)

addressAsNibbleString::Address->N.NibbleString
addressAsNibbleString (Address s) = N.EvenNibbleString $ BL.toStrict $ encode s

getAddressState::Address->ContextM AddressState
getAddressState address = do
  states <- getKeyVals $ addressAsNibbleString address
  case states of
    [] -> return blankAddressState
    [state] -> return $ rlpDecode $ rlpDeserialize $ rlpDecode $ snd state
    _ -> error ("getAddressStates found multiple states for: " ++ show (pretty address) ++ "\n" ++ intercalate "\n" (show . pretty <$> states))
  

getAllAddressStates::ContextM [(N.NibbleString, AddressState)]
getAllAddressStates = do
  states <- getKeyVals ""
  return $ fmap rlpDecode <$> states

  

putAddressState::Address->AddressState->ContextM ()
putAddressState address newState = 
  putKeyVal (addressAsNibbleString address) $ rlpEncode $ rlpSerialize $ rlpEncode newState

