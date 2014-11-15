
module JCommand (
                 JCommand(..),
                 Storage(..),
                 Word(..),
                 JBool(..),
                 jcompile
                ) where

import Prelude hiding (LT)

import Data.Functor
import Util
import VM.Opcodes

import ExtWord

data Storage = PermStorage Word | MemStorage Word deriving (Show)

data Word = Number Word256 | TheAddress | Origin | Caller | Input Word256 | PermVal Word | MemVal Word | Word :+: Word | Word :-: Word deriving (Show)

data JBool = JTrue | JFalse | Word :>=: Word deriving (Show)

--instance Num Word where
--    isNum x = undefined

data JCommand = Storage :=: Word | If JBool [JCommand] | ReturnCode [JCommand] deriving (Show)

infixl 6 :+:
infixl 5 :-:
infixl 4 :=:


j::[JCommand]->Unique [Operation]
j x = fmap concat $ sequence $ jCommand2Op <$> x

jcompile x = runUnique (j x) 0

data Unique a = Unique { runUnique::Int->(Int, a) }

instance Functor Unique where
    fmap f' Unique{runUnique=f} = Unique (fmap f' . f)

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
pushVal Origin = [ORIGIN]
pushVal (Input x) = [PUSH $ integer2Bytes1 $ toInteger (32*x), CALLDATALOAD]
pushVal (PermVal x) = pushVal x ++ [SLOAD]
pushVal (MemVal x) = pushVal x ++ [MLOAD]
pushVal (x :+: y) = pushVal y ++ pushVal x ++ [ADD]
pushVal (x :-: y) = pushVal y ++ pushVal x ++ [SUB]

pushBoolVal::JBool->[Operation]
pushBoolVal (x :>=: y) = pushVal y ++ pushVal x ++ [NOT, LT]
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
    return $ pushBoolVal cond ++ [NOT, PUSHLABEL after, JUMPI] ++ compiledCode ++ [LABEL after]
jCommand2Op (ReturnCode code) = do
  codeBegin <- getUnique "begin"
  codeEnd <- getUnique "end"
  compiledCode <- j code
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
             ++ [LABEL codeBegin] ++ compiledCode ++ [LABEL codeEnd]
