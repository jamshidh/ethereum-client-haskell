{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Data.AddressState (
  AddressState(..),
  blankAddressState,
  getAddressState,
  getAllAddressStates,
  putAddressState,
  deleteAddressState,
  addressStateExists
  ) where

import Data.Binary
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.Address
import qualified Blockchain.Colors as CL
import Blockchain.Context
import Blockchain.ExtDBs
import Blockchain.Format
import qualified Data.NibbleString as N
import Blockchain.Data.RLP
import Blockchain.SHA
import Blockchain.Util

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
  --rlpEncode a | balance a < 0 = rlpEncode a{balance = - balance a}
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
    [] -> do
      putAddressState address blankAddressState
      return blankAddressState
    [state] -> return $ rlpDecode $ rlpDeserialize $ rlpDecode $ snd state
    _ -> error ("getAddressStates found multiple states for: " ++ show (pretty address) ++ "\n" ++ intercalate "\n" (show . pretty <$> states))
  

getAllAddressStates::ContextM [(N.NibbleString, AddressState)]
getAllAddressStates = do
  states <- getKeyVals ""
  return $ fmap (rlpDecode . rlpDeserialize . rlpDecode) <$> states

  

putAddressState::Address->AddressState->ContextM ()
putAddressState address newState = 
  putKeyVal (addressAsNibbleString address) $ rlpEncode $ rlpSerialize $ rlpEncode newState

deleteAddressState::Address->ContextM ()
deleteAddressState address = 
  deleteKey (addressAsNibbleString address)

addressStateExists::Address->ContextM Bool
addressStateExists address = 
  keyExists (addressAsNibbleString address)

