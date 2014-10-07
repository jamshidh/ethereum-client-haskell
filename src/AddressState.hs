{-# LANGUAGE OverloadedStrings #-}

module AddressState (
  AddressState(..),
  getAddressState,
  getAllAddressStates,
  putAddressState
  ) where

import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import Data.Functor
import Database.LevelDB
import Network.Haskoin.Internals hiding (Address)
import Numeric

import Address
import Colors
import EthDB
import Format
import qualified NibbleString as N
import RLP
import SHA
import Util

data AddressState = AddressState { addressStateNonce::Word256, balance::Word256, contractRoot::Word256, codeHash::SHA } deriving (Show)

instance Format AddressState where
  format a = blue "AddressState" ++
             tab("\nnonce: " ++ showHex (addressStateNonce a) "" ++
                 "\nbalance: " ++ show (fromIntegral $ balance a) ++
                 "\ncontractRoot: " ++ showHex (contractRoot a) "" ++
                 "\ncodeHash: " ++ format (codeHash a))
  
instance RLPSerializable AddressState where
  rlpEncode a = RLPArray [rlpNumber $ fromIntegral $ addressStateNonce a, rlpNumber $ fromIntegral $ balance a, rlpNumber $ fromIntegral $ contractRoot a, rlpEncode $ codeHash a]

  rlpDecode (RLPArray [n, b, cr, ch]) =
    AddressState {
      addressStateNonce=fromIntegral $ getNumber n,
      balance=fromIntegral $ getNumber b,
      contractRoot=fromIntegral $ getNumber cr,
      codeHash=rlpDecode ch
      } 

addressAsNibbleString::Address->N.NibbleString
addressAsNibbleString (Address s) = N.EvenNibbleString $ B.pack $ integer2Bytes $ fromIntegral s

getAddressState::DB->SHAPtr->Address->ResourceT IO AddressState
getAddressState db p address = do
  states <- getKeyVals db p $ addressAsNibbleString address
  case states of
    [] -> error ("getAddressStates found no state for: " ++ format address)
    [state] -> return $ rlpDecode $ rlpDeserialize $ snd state
    x -> error ("getAddressStates found multiple states for: " ++ format address)
  

getAllAddressStates::DB->SHAPtr->ResourceT IO [(N.NibbleString, AddressState)]
getAllAddressStates db p = do
  states <- getKeyVals db p ""
  return $ fmap (rlpDecode . rlpDeserialize) <$> states

  

putAddressState::DB->SHAPtr->Address->AddressState->ResourceT IO SHAPtr
putAddressState db p address newState = do
  putKeyVal db p (addressAsNibbleString address) (rlpSerialize $ rlpEncode newState)

