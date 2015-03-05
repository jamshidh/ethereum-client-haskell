
module Blockchain.JCommand (
                 JCommand(..),
                 Storage(..),
                 Word(..),
                 JBool(..),
                 jcompile
                ) where

import Prelude hiding (LT, GT, EQ)

import Control.Applicative
import Control.Monad

import Blockchain.Data.Code
import Blockchain.Util
import Blockchain.VM.Opcodes
import Blockchain.VM.Code

import Blockchain.ExtWord

data Storage = PermStorage Word | MemStorage Word deriving (Show)

data Word = 
    Number Word256 | 
    TheAddress | 
    Origin | 
    Caller | 
    CallDataSize | 
    Input Word | 
    PermVal Word | 
    MemVal Word | 
    Abs Word | 
    Word :+: Word | Word :-: Word | Word :*: Word | Neg Word | Signum Word deriving (Show)

data JBool = JTrue | JFalse | 
             Word :==: Word | 
             Word :>: Word | 
             Word :<: Word |
             Word :>=: Word | 
             Word :<=: Word 
                  deriving (Show)

instance Num Word where
    fromInteger x = Number $ fromInteger x
    Number x + Number y = Number $ x+y
    x + y = x :+: y
    x - y = x :-: y
    Number x * Number y = Number $ x*y
    x * y = x :*: y
    abs (Number x) = Number $ abs x
    abs x = Abs x
    negate (Number x) = Number (-x)
    negate x = Neg x

    signum (Number x) = Number (signum x)
    signum x = Signum x


data JCommand = Storage :=: Word | 
                If JBool [JCommand] | 
                While JBool [JCommand] | 
                ReturnCode Code deriving (Show)

infixl 6 :+:
infixl 5 :-:
infixl 4 :=:


j::[JCommand]->Unique [Operation]
j x = fmap concat $ sequence $ jCommand2Op <$> x

jcompile::[JCommand]->(Int, [Operation])
jcompile x = runUnique (j x) 0

data Unique a = Unique { runUnique::Int->(Int, a) }

instance Functor Unique where
    fmap = liftM

instance Applicative Unique where
    pure = return
    (<*>) = ap

instance Monad Unique where
    (Unique runner) >>= f = Unique $ \val -> let (val', x') = runner val
                                                 Unique g = f x'
                                             in g val'
    return x = Unique $ \val -> (val, x)

getUnique::String->Unique String
getUnique s = Unique $ \val -> (val+1, s ++ show val)

pushVal::Word->[Operation]
pushVal (Number x) = [PUSH $ integer2Bytes1 $ toInteger x]
pushVal TheAddress = [ADDRESS]
pushVal Caller = [CALLER]
pushVal CallDataSize = [CALLDATASIZE]
pushVal Origin = [ORIGIN]
pushVal (Input x) = pushVal x ++ [CALLDATALOAD]
pushVal (PermVal x) = pushVal x ++ [SLOAD]
pushVal (MemVal x) = pushVal x ++ [MLOAD]
pushVal (x :+: y) = pushVal y ++ pushVal x ++ [ADD]
pushVal (x :-: y) = pushVal y ++ pushVal x ++ [SUB]
pushVal (x :*: y) = pushVal y ++ pushVal x ++ [MUL]
pushVal (Abs x) = pushVal x ++ pushVal (Signum x) ++ [MUL]
pushVal (Signum x) = pushVal x ++ pushVal (Number 0) ++ [GT] ++ pushVal x ++ pushVal (Number 0) ++ [LT, SUB]
pushVal (Neg x) = pushVal x ++ [NEG]

pushBoolVal::JBool->[Operation]
pushBoolVal (x :==: y) = pushVal y ++ pushVal x ++ [EQ]
pushBoolVal (x :>: y) = pushVal y ++ pushVal x ++ [GT]
pushBoolVal (x :<: y) = pushVal y ++ pushVal x ++ [LT]
pushBoolVal (x :>=: y) = pushVal y ++ pushVal x ++ [ISZERO, LT]
pushBoolVal (x :<=: y) = pushVal y ++ pushVal x ++ [ISZERO, GT]
pushBoolVal JTrue = [PUSH [1]]
pushBoolVal JFalse = [PUSH [0]]

jCommand2Op::JCommand->Unique [Operation]
jCommand2Op (PermStorage sPosition :=: val) = 
    return $ pushVal val ++ pushVal sPosition ++ [SSTORE]
jCommand2Op (MemStorage sPosition :=: val) = 
    return $ pushVal val ++ pushVal sPosition ++ [MSTORE]
jCommand2Op (If cond code) = do
    after <- getUnique "after"
    compiledCode <- j code
    return $ pushBoolVal cond ++ [ISZERO, PUSHLABEL after, JUMPI] ++ compiledCode ++ [LABEL after]
jCommand2Op (While cond code) = do
    after <- getUnique "after"
    before <- getUnique "before"
    compiledCode <- j code
    return $ [LABEL before] ++ pushBoolVal cond ++ [ISZERO, PUSHLABEL after, JUMPI] ++ compiledCode ++ [PUSHLABEL before, JUMP] ++ [LABEL after]
jCommand2Op (ReturnCode (Code codeBytes)) = do
  codeBegin <- getUnique "begin"
  codeEnd <- getUnique "end"
  return $ 
             [ 
              PUSHDIFF codeBegin codeEnd,
              PUSHLABEL codeBegin,
              PUSH [0],
              CODECOPY,
              PUSHDIFF codeBegin codeEnd,
              PUSH [0],
              RETURN
             ]
             ++ [LABEL codeBegin, DATA codeBytes, LABEL codeEnd]
