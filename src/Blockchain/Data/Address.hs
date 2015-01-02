
module Blockchain.Data.Address (
  Address(..),
  prvKey2Address,
  pubKey2Address,
  getNewAddress
  ) where

import Control.Monad
import qualified Crypto.Hash.SHA3 as C
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe
import Network.Haskoin.Crypto hiding (Address)
import Network.Haskoin.Internals hiding (Address)
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.RLP
import Blockchain.SHA
import Blockchain.Util

newtype Address = Address Word160 deriving (Show, Eq)

instance Pretty Address where
  pretty (Address x) = yellow $ text $ padZeros 40 $ showHex x ""

instance Binary Address where
  put (Address x) = sequence_ $ fmap put $ word160ToBytes $ fromIntegral x
  get = do
    bytes <- replicateM 20 get
    let byteString = B.pack bytes
    return (Address $ fromInteger $ byteString2Integer byteString)


prvKey2Address::PrvKey->Address
prvKey2Address prvKey =
  Address $ fromInteger $ byteString2Integer $ C.hash 256 $ BL.toStrict $ encode x `BL.append` encode y
  --B16.encode $ hash 256 $ BL.toStrict $ encode x `BL.append` encode y
  where
    PubKey point = derivePubKey prvKey
    x = fromMaybe (error "getX failed in prvKey2Address") $ getX point
    y = fromMaybe (error "getY failed in prvKey2Address") $ getY point

pubKey2Address::PubKey->Address
pubKey2Address (PubKey point) =
  Address $ fromInteger $ byteString2Integer $ C.hash 256 $ BL.toStrict $ encode x `BL.append` encode y
  --B16.encode $ hash 256 $ BL.toStrict $ encode x `BL.append` encode y
  where
    x = fromMaybe (error "getX failed in prvKey2Address") $ getX point
    y = fromMaybe (error "getY failed in prvKey2Address") $ getY point
pubKey2Address (PubKeyU _) = error "Missing case in pubKey2Address: PubKeyU"


instance RLPSerializable Address where
  rlpEncode (Address a) = RLPString $ BLC.unpack $ encode a
  rlpDecode (RLPString s) = Address $ decode $ BLC.pack s
  rlpDecode x = error ("Malformed rlp object sent to rlp2Address: " ++ show x)

getNewAddress::Address->Integer->Address
getNewAddress a n =
    let theHash = hash $ rlpSerialize $ RLPArray [rlpEncode a, rlpEncode n]
    in decode $ BL.drop 12 $ encode theHash

