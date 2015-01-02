
module Decompile2
    (
     decompile,
     formatSequence
    ) where

import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Maybe

import Blockchain.VM.Code
import Blockchain.VM.Opcodes

import Blockchain.Util

import Debug.Trace

data Sequence = Sequence Integer [Command] deriving (Show, Eq)

data Expression = Number Integer | Variable String | Unary String Expression | Binary String Expression Expression | Function String [Expression] deriving (Show, Eq)

data Command = OpCommand Operation | JumpDest Integer | Command Expression | Put Expression | StackFunction String Int Int | Subroutine Integer deriving (Show, Eq)

getSequence::Integer->Code->Sequence
getSequence p code = Sequence p $ code2Commands p code

op2Command::Operation->Command
op2Command CALLER = Put $ Variable "caller"
op2Command GAS = Put $ Variable "gas"
op2Command (PUSH x) = Put $ Number $ bytes2Integer x
op2Command JUMP = StackFunction "jump" 1 0
op2Command CODECOPY = StackFunction "codecopy" 3 0
op2Command CALL = StackFunction "call" 7 1
op2Command SSTORE = StackFunction "sstore" 2 0
op2Command MSTORE = StackFunction "mstore" 2 0
op2Command RETURN = StackFunction "return" 2 0
op2Command EXP = StackFunction "exp" 2 1
op2Command SUB = StackFunction "sub" 2 1
op2Command AND = StackFunction "and" 2 1
op2Command MLOAD = StackFunction "mload" 1 1
op2Command op = OpCommand op

code2Commands::Integer->Code->[Command]
code2Commands i o | op == JUMPDEST = JumpDest i:code2Commands (i+fromIntegral opSize) o
                  | op == STOP = [op2Command STOP]
                  | op == RETURN = [op2Command RETURN]
                  | otherwise = op2Command op:code2Commands (i+fromIntegral opSize) o
    where 
      (op, opSize) = getOperationAt o (fromIntegral i)

hasNoSideEffects::Expression->Bool
hasNoSideEffects (Function "call" _) = False
hasNoSideEffects x = error $ "Missing case in hasNoSideEffects: " ++ show x

simplifyCommands::Code->[Command]->[Command]
simplifyCommands code (c@(Command (Function "jump" [Number loc])):rest) | not (codeReturns code loc) = [c]
simplifyCommands code (Put (Number loc1):Command (Function "jump" [Number loc]):JumpDest loc2:rest) | loc1 == loc2 && codeReturns code loc = Subroutine loc:simplifyCommands code rest
simplifyCommands code (Put x:StackFunction name 1 0:rest) = Command (Function name [x]):simplifyCommands code rest
simplifyCommands code (Put x:StackFunction name 1 1:rest) = Put (Function name [x]):simplifyCommands code rest
simplifyCommands code (Put x:Put y:StackFunction name 2 1:rest) = Put (Function name [y, x]):simplifyCommands code rest
simplifyCommands code (Put x:Put y:StackFunction name 2 0:rest) = Command (Function name [y, x]):simplifyCommands code rest
simplifyCommands code (Put v1:Put v2:Put v3:StackFunction name 3 1:rest) = Put (Function name [v3, v2, v1]):simplifyCommands code rest
simplifyCommands code (Put v1:Put v2:Put v3:StackFunction name 3 0:rest) = Command (Function name [v3, v2, v1]):simplifyCommands code rest
simplifyCommands code (Put v1:Put v2:Put v3:Put v4:Put v5:Put v6:Put v7:StackFunction name 7 1:rest) = Put (Function name [v7, v6, v5, v4, v3, v2, v1]):simplifyCommands code rest
simplifyCommands code (Put x:OpCommand DUP1:rest) = Put x:Put x:simplifyCommands code rest
simplifyCommands code (Put x:Put y:OpCommand DUP2:rest) = Put y:Put x:Put y:simplifyCommands code rest
simplifyCommands code (Put x:Put y:OpCommand SWAP1:rest) = Put y:Put x:simplifyCommands code rest
simplifyCommands code (Put y:OpCommand POP:rest) | hasNoSideEffects y = simplifyCommands code rest
simplifyCommands code (Put (Number x):cmd@(Command c):rest) | not (isJump cmd) = Command c:Put (Number x):simplifyCommands code rest
simplifyCommands _ [] = []
simplifyCommands code (x:rest) = x:simplifyCommands code rest

simplify::Code->Sequence->Sequence
simplify code (Sequence location commands) = Sequence location $ simplifyCommands code commands

formatExpression::Expression->String
formatExpression (Variable name) = "$" ++ name
formatExpression (Number x) = show x
formatExpression (Function "sload" [p]) = "store[" ++ formatExpression p ++ "]"
formatExpression (Function "mload" [p]) = "mem[" ++ formatExpression p ++ "]"
formatExpression (Function "sstore" [key, val]) = "store[" ++ formatExpression key ++ "] = " ++ formatExpression val 
formatExpression (Function "mstore" [key, val]) = "mem[" ++ formatExpression key ++ "] = " ++ formatExpression val 
formatExpression (Function "exp" [v1, v2]) = formatExpression v1 ++ "^" ++ formatExpression v2
formatExpression (Function "sub" [v1, v2]) = formatExpression v1 ++ " - " ++ formatExpression v2
formatExpression (Function "and" [v1, v2]) = formatExpression v1 ++ " AND " ++ formatExpression v2
formatExpression (Function name params) = name ++ "(" ++ intercalate ", " (map formatExpression params) ++ ")"
formatExpression x = show x

formatCommand::Command->String
formatCommand (Put x) = "put " ++ formatExpression x
formatCommand (Command x) = formatExpression x
formatCommand x = show x

formatSequence::Sequence->String
formatSequence (Sequence location commands) = "Sequence at " ++ show location ++ "\n" ++ unlines (map formatCommand commands)

countMap = M.fromList $ (\(OPData _ op stackOut stackIn _) -> (op, (stackOut, stackIn))) <$> opDatas

getOpCount::Operation->(Int, Int)
getOpCount (PUSH _) = (0, 1)
getOpCount x = fromMaybe (error $ "Missing value in countMap: " ++ show x) $ M.lookup x countMap
-- getOpCount x = error $ "Missing case in getOpCount: " ++ show x

codeReturns'::Code->Integer->Integer->Bool
codeReturns' code p count | op == JUMP = trace ("            the count is: " ++ show count) $ count == 0
                          | op == STOP = False
                          | otherwise = let (use, ret) = getOpCount op in codeReturns' code (p + fromIntegral size) (count + toInteger use - toInteger ret)
    where 
      (op, size) = getOperationAt code $ fromIntegral p

codeReturns::Code->Integer->Bool
codeReturns code p = trace ("             pppppppppppppppppppppppp: " ++ show x) $ x where x = codeReturns' code p 0

converge::Eq a=>(a->a)->a->a
converge f x | f x == x = x
             | otherwise = converge f (f x)

decompileSequence::Code->Integer->Sequence
decompileSequence c p = converge (simplify c) $ getSequence p c
--decompileSequence c p = simplify c $ simplify c $ simplify c $ simplify c $ simplify c $ getSequence p c -- converge (simplify c) $ getSequence p c

getJumpLocations::Sequence->[Integer]
getJumpLocations (Sequence _ commands) = getJumpLocations' commands
    where
      getJumpLocations'::[Command]->[Integer]
      getJumpLocations' [] = []
      getJumpLocations' (Command (Function "jump" [Number loc]):rest) = loc:getJumpLocations' rest
      getJumpLocations' (Subroutine loc:rest) = loc:getJumpLocations' rest
      getJumpLocations' (_:rest) = getJumpLocations' rest

loc::Sequence->Integer
loc (Sequence l _) = l

commands::Sequence->[Command]
commands (Sequence _ c) = c

getMissingLocations::[Sequence]->[Integer]
getMissingLocations seqs = (seqs >>= getJumpLocations) \\ map loc seqs

addMissingSequencesPartial::Code->[Sequence]->[Sequence]
addMissingSequencesPartial c seqs = seqs ++ map (decompileSequence c) (getMissingLocations seqs)

isJump::Command->Bool
isJump (Command (Function "jump" _)) = True
isJump _ = False

isJumpDest::Command->Bool
isJumpDest (JumpDest _) = True
isJumpDest _ = False



simplifySeqs::Code->[Sequence]->[Sequence]
simplifySeqs code [seq1@(Sequence l1 c1), seq2@(Sequence l2 (jd:rest2))] 
    | length (getJumpLocations seq1) == 1 && length (getJumpLocations seq2) == 0 
      && isJumpDest jd && isJump (last c1)
          = [converge (simplify code) $ Sequence (loc seq1) (init c1 ++ rest2)]
simplifySeqs _ seqs = seqs

decompile::Code->[Sequence]
decompile c = theSeqs -- simplifySeqs c theSeqs
    where
      theSeqs = converge addNeededSeqs [firstSeq]
      firstSeq = decompileSequence c 0
      addNeededSeqs = addMissingSequencesPartial c

