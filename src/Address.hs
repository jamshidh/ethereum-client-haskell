
module Address (
  Address(..),
  prvKey2Address,
  rlp2Address,
  address2RLP
  ) where

import Crypto.Hash.SHA3
import Data.Binary
import Data.ByteString.Internal
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Network.Haskoin.Crypto hiding (Address)
import Network.Haskoin.Internals hiding (Address)
import Numeric

import Colors
import Format
import RLP
import Util

newtype Address = Address Word160 deriving (Show, Eq)

instance Format Address where
  format (Address x) = yellow $ padZeros 20 $ showHex x ""


--TODO- fix the signature of this to create an Address
prvKey2Address::PrvKey->ByteString
prvKey2Address prvKey =
  B16.encode $ hash 256 $ BL.toStrict $ encode x `BL.append` encode y
  where
    PubKey point = derivePubKey prvKey
    x =
      case getX point of
        Just val -> val
        _ -> error "getX failed in prvKey2Address"
    y =
      case getY point of
        Just val -> val
        _ -> error "getY failed in prvKey2Address"


address2RLP::Address->RLPObject
address2RLP (Address a) = RLPString $ BLC.unpack $ encode a

rlp2Address::RLPObject->Address
rlp2Address (RLPString s) = Address $ decode $ BLC.pack s
