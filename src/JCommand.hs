
module JCommand (
                 JCommand(..),
                 Storage(..),
                 Word(..),
                 JBool(..),
                 j
                ) where

import Prelude hiding (LT)

import Data.Functor
import Util
import VM.Opcodes

import ExtWord

data Storage = PermStorage Word | MemStorage Word

data Word = Number Word256 | TheAddress | Origin | Caller | Input Word256 | PermVal Word | MemVal Word | Word :-: Word

data JBool = JTrue | JFalse | Word :>=: Word

--instance Num Word where
--    isNum x = undefined

data JCommand = Storage :=: Word | If JBool [JCommand]

infixl 5 :-:
infixl 4 :=:


j::[JCommand]->[Operation]
j x = concat $ jCommand2Op <$> x

pushVal::Word->[Operation]
pushVal (Number x) = [PUSH $ integer2Bytes1 $ toInteger x]
pushVal TheAddress = [ADDRESS]
pushVal Caller = [CALLER]
pushVal Origin = [ORIGIN]
pushVal (Input x) = [PUSH $ integer2Bytes1 $ toInteger x, CALLDATALOAD]
pushVal (PermVal x) = pushVal x ++ [SLOAD]
pushVal (MemVal x) = pushVal x ++ [MLOAD]
pushVal (x :-: y) = pushVal y ++ pushVal x ++ [SUB]

pushBoolVal::JBool->[Operation]
pushBoolVal (x :>=: y) = pushVal y ++ pushVal x ++ [NOT, LT]
pushBoolVal JTrue = [PUSH [1]]
pushBoolVal JFalse = [PUSH [0]]

jCommand2Op::JCommand->[Operation]
jCommand2Op (PermStorage sPosition :=: val) = 
    pushVal val ++ pushVal sPosition ++ [SSTORE]
jCommand2Op (MemStorage sPosition :=: val) = 
    pushVal val ++ pushVal sPosition ++ [MSTORE]
jCommand2Op (If cond code) = 
    pushBoolVal cond ++ [NOT, RelativeLoc $ sum $ opLen <$> codeOps, JUMPI] ++ codeOps
        where codeOps = jCommand2Op =<< code
