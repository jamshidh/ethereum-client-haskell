{-# LANGUAGE OverloadedStrings #-}

module Data.AddressState (
  AddressState(..),
  getAddressState,
  getAllAddressStates,
  putAddressState
  ) where

import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import Data.Functor
import Numeric
import Text.PrettyPrint.Leijen hiding ((<$>))

import Data.Address
import Colors
import Database.DBs
import Database.MerklePatricia
import Format
import qualified Data.NibbleString as N
import Data.RLP
import SHA
import Util

data AddressState = AddressState { addressStateNonce::Integer, balance::Integer, contractRoot::Maybe SHAPtr, codeHash::SHA } deriving (Show)

instance Format AddressState where
  format a = blue "AddressState" ++
             tab("\nnonce: " ++ showHex (addressStateNonce a) "" ++
                 "\nbalance: " ++ show (toInteger $ balance a) ++
                 "\ncontractRoot: " ++ formatCR (contractRoot a) ++
                 "\ncodeHash: " ++ format (codeHash a))
    where
      formatCR::Maybe SHAPtr->String
      formatCR Nothing = "0x0"
      formatCR (Just x) = format x
  
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

getAddressState::DB->Address->ResourceT IO (Maybe AddressState)
getAddressState db address = do
  states <- getKeyVals db $ addressAsNibbleString address
  case states of
    [] -> return Nothing
    [state] -> return $ Just $ rlpDecode $ rlpDeserialize $ rlpDecode $ snd state
    _ -> error ("getAddressStates found multiple states for: " ++ format address)
  

getAllAddressStates::DB->ResourceT IO [(N.NibbleString, AddressState)]
getAllAddressStates db = do
  states <- getKeyVals db ""
  return $ fmap rlpDecode <$> states

  

putAddressState::DB->Address->AddressState->ResourceT IO DB
putAddressState db address newState = do
  putKeyVal db (addressAsNibbleString address) $ rlpEncode $ rlpSerialize $ rlpEncode newState

