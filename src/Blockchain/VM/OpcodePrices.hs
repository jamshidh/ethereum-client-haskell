{-# OPTIONS_GHC  -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module Blockchain.VM.OpcodePrices (
  OpGasItem (..),
  opGasItemPrice,
  opGasPriceAndRefund,
  countBytesToWords
  ) where

import Prelude hiding (EQ, LT, GT)

import Control.Monad( )
import Control.Monad.Trans.Class
import Data.Bits

import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.ExtWord
import Blockchain.VM.Opcodes
import Blockchain.VM.VMM

opGasPrice::Operation->Integer

opGasPrice (MalformedOpcode _) = 0 --gonna fail anyway, just put something arbitrary here

opGasPrice op
  = case op of
    DUP1         -> 3
    DUP2         -> 3
    DUP3         -> 3
    DUP4         -> 3
    DUP5         -> 3
    DUP6         -> 3
    DUP7         -> 3
    DUP8         -> 3
    DUP9         -> 3
    DUP10        -> 3
    DUP11        -> 3
    DUP12        -> 3
    DUP13        -> 3
    DUP14        -> 3
    DUP15        -> 3
    DUP16        -> 3
    SWAP1        -> 3
    SWAP2        -> 3
    SWAP3        -> 3
    SWAP4        -> 3
    SWAP5        -> 3
    SWAP6        -> 3
    SWAP7        -> 3
    SWAP8        -> 3
    SWAP9        -> 3
    SWAP10       -> 3
    SWAP11       -> 3
    SWAP12       -> 3
    SWAP13       -> 3
    SWAP14       -> 3
    SWAP15       -> 3
    SWAP16       -> 3
    (PUSH _)     -> 3
    ADD          -> 3
    MUL          -> 5
    SUB          -> 3
    DIV          -> 5
    SDIV         -> 5
    MOD          -> 5
    SMOD         -> 5
    ADDMOD       -> 8
    MULMOD       -> 8
    SIGNEXTEND   -> 5
    LT           -> 3
    GT           -> 3
    SLT          -> 3
    SGT          -> 3
    EQ           -> 3
    ISZERO       -> 3
    AND          -> 3
    OR           -> 3
    XOR          -> 3
    NOT          -> 3
    BYTE         -> 3
    ADDRESS      -> 2
    BALANCE      -> 20
    ORIGIN       -> 2
    CALLER       -> 2
    CALLVALUE    -> 2
    CALLDATALOAD -> 3
    CALLDATASIZE -> 2
    CODESIZE     -> 2
    GASPRICE     -> 2
    EXTCODESIZE  -> 20
    BLOCKHASH    -> 20
    COINBASE     -> 2
    TIMESTAMP    -> 2
    NUMBER       -> 2
    DIFFICULTY   -> 2
    GASLIMIT     -> 2
    POP          -> 2
    MLOAD        -> 3
    MSTORE       -> 3
    MSTORE8      -> 3
    SLOAD        -> 50
    JUMP         -> 8
    JUMPI        -> 10
    PC           -> 2
    MSIZE        -> 2
    GAS          -> 2
    JUMPDEST     -> 1
    CREATE       -> 32000
    CALLCODE     -> 40
    RETURN       -> 0
    STOP         -> 0
    SUICIDE      -> 0
    x            -> error $ "Missing opcode in opCodePrice: " ++ show x

----------------------

data OpGasItem
  = TXBASE | TXDATANONZERO | TXDATAZERO
  | MEMWORD | QUADCOEFFDIV
  | EXPBASE | EXPBYTE
  | CALLDATACOPYBASE | CODECOPYBASE | EXTCODECOPYBASE | COPYWORD
  | LOGBASE | LOGTOPIC | LOGDATA
  | CALLBASE | CALLVALUETRANSFER | CALLSTIPEND | CALLNEWACCOUNT
  | CREATEDATA
  | SHA3BASE | SHA3WORD | SHA256BASE | SHA256WORD | ECRECOVER
  | RIPEMD160BASE | RIPEMD160WORD
  | IDENTITYBASE | IDENTITYWORD

opGasItemPrice :: OpGasItem -> Integer

opGasItemPrice item
  = case item of
    TXBASE            -> 21000
    TXDATANONZERO     -> 68
    TXDATAZERO        -> 4
    MEMWORD           -> 3
    QUADCOEFFDIV      -> 512
    EXPBASE           -> 10
    EXPBYTE           -> 10
    CALLDATACOPYBASE  -> 3
    CODECOPYBASE      -> 3
    EXTCODECOPYBASE   -> 20
    COPYWORD          -> 3
    LOGBASE           -> 375
    LOGTOPIC          -> 375
    LOGDATA           -> 8
    CALLBASE          -> 40
    CALLVALUETRANSFER -> 9000  -- ::Word256 -- really?
    CALLSTIPEND       -> 2300  -- ::Word256 -- really?
    CALLNEWACCOUNT    -> 25000 -- ::Word256 -- really?
    CREATEDATA        -> 200
    SHA3BASE          -> 30
    SHA3WORD          -> 6
    ECRECOVER         -> 3000
    SHA256BASE        -> 60
    SHA256WORD        -> 12
    RIPEMD160BASE     -> 600
    RIPEMD160WORD     -> 120
    IDENTITYBASE      -> 15
    IDENTITYWORD      -> 3

-------------------

opGasPriceAndRefund::Operation->VMM (Integer, Integer)

opGasPriceAndRefund SUICIDE = return (0, 24000)

opGasPriceAndRefund SSTORE = do
  p <- getStackItem 0
  val <- getStackItem 1
  oldVal <- getStorageKeyVal p
  case (oldVal, val) of
      (0, x) | x /= (0::Word256) -> return (20000, 0)
      (x, 0) | x /= 0 -> return (5000, 15000)
      _ -> return (5000, 0)
      
opGasPriceAndRefund x = do
  price <- priceM
  return (price, 0)
    where
      priceM =
        let g = opGasItemPrice
        in case x of
          SHA3 -> simplePrice 1 30 6 countBytesToWords
          EXP  ->
            let bytesNeeded = bytesNeeded' 0
                bytesNeeded' n 0 = n
                bytesNeeded' n y = bytesNeeded' (n + 1) (y `shiftR` 8)
            in  simplePrice 1 (g EXPBASE) (g EXPBYTE) bytesNeeded
          LOG0 -> simplePriceLOG 0
          LOG1 -> simplePriceLOG 1
          LOG2 -> simplePriceLOG 2
          LOG3 -> simplePriceLOG 3
          LOG4 -> simplePriceLOG 4
          CODECOPY
               -> simplePriceCOPY 2 (g CODECOPYBASE)
          CALLDATACOPY
               -> simplePriceCOPY 2 (g CALLDATACOPYBASE)
          EXTCODECOPY
               -> simplePriceCOPY 3 (g EXTCODECOPYBASE)
          CALL -> priceCALL True
          CALLCODE
               -> priceCALL False
          y    -> return $! opGasPrice y

simplePrice stackItem gasBase gasScale word256Weight
  = let price size = gasBase + gasScale * (fromIntegral $ word256Weight size)
    in do
       size <- getStackItem stackItem :: VMM Word256
       return $! price size

--missing stuff
--Glog 1 Partial payment for a LOG operation.
--Glogdata 1 Paid for each byte in a LOG operation’s data.
--Glogtopic 1 Paid for each topic of a LOG operation.      
simplePriceLOG n
  = let g = opGasItemPrice
    in simplePrice 1 (g LOGBASE + n * (g LOGTOPIC)) (g LOGDATA) id
       
simplePriceCOPY n base
  = let g = opGasItemPrice
    in simplePrice n base (g COPYWORD) countBytesToWords
    
priceCALL b
  = do
    gas <- getStackItem 0 :: VMM Word256
    toAccountExists <-
      if b
      then (getStackItem 1::VMM Word256) >>=
           (lift . lift . lift . addressStateExists . Address . fromIntegral)
      else return True
    val <- getStackItem 2::VMM Word256
    
    let g = opGasItemPrice in
      return $!
       (fromIntegral gas) + (g CALLBASE) +
      (if toAccountExists
       then 0
       else (g CALLNEWACCOUNT)) +
      (if val > 0
       then (g CALLVALUETRANSFER)
       else 0)

countBytesToWords :: (Integral a) => a -> Integer
countBytesToWords nbytes
  | nbytes >= 0 =
    let (q32, r32) = nbytes `quotRem` 32
        roundUp = if r32 == 0 then 0 else 1
    in fromIntegral $ q32 + roundUp
  | otherwise = undefined
