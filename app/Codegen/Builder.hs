{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen.Builder where

import Control.Monad.State
import Control.Monad.Fix (MonadFix)
import Control.Applicative ((<$>))

import Data.Maybe

import LLVM.AST hiding (function, alignment, Call)
import LLVM.AST.ParameterAttribute (ParameterAttribute)
import qualified LLVM.AST.IntegerPredicate as IPredicats
import qualified LLVM.AST.FloatingPointPredicate as FPredicats

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction hiding (load, store)

import AST.Utils
import StringUtils
import Syntax

import Codegen.Primitives
import Codegen.ASTBridge

data Context = Context { target :: Operand
                       } deriving (Show)

type CompilationState = State Context

extractDefs :: MonadIRBuilder m => [Expr] -> m ()
extractDefs (expr:exprs) = do
  case expr of
    (BinaryOp "=" maybeDef _) ->  case maybeDef of
      Def defType defName -> do
        allocateDef maybeDef
        pure ()
      _ -> pure ()
    _ -> pure ()
  extractDefs exprs
extractDefs [] = pure ()

buildCodeBlock :: (MonadFix m, MonadIRBuilder m) => [Expr] -> m Operand
emit :: (MonadFix m, MonadIRBuilder m) => Expr -> m Operand

emit (Block codeBlock) = buildCodeBlock codeBlock

emit (BinaryOp "=" dest object) =
  do
    value <- emit object
    store (referenceIntPointer name) value
    return value -- Kinda like C++ '='
  where
    name = case dest of 
      Def _ n -> n
      Var n -> n

-- TODO: UnaryOp
emit (BinaryOp operator opr1 opr2) = 
  do
    operand1 <- emit opr1
    operand2 <- emit opr2
    operation operand1 operand2
  where
    operation = case operator of
      "+" -> add
      "-" -> sub
      "*" -> mul
      --"/" -> div
      -- TODO: support float
      "<" -> icmp IPredicats.SLT
      ">" -> icmp IPredicats.SGT
      "==" -> icmp IPredicats.EQ
      "!=" -> icmp IPredicats.NE
      "<=" -> icmp IPredicats.SLE
      ">=" -> icmp IPredicats.SGE

emit (Call funcName exprs) =
  do
    args <- emitArgs exprs
    call (makeFuncRef funcName) args
  where
    emitArgs (e:es) = do
      arg <- emit e
      args <- emitArgs es
      return ((arg, []) : args)
    emitArgs _ = return []

emit (If cond blockTrue blockFalse) = mdo
  condition <- emit cond
  resultPointer <- allocateInt -- TODO: type inference or typed ints 
  condBr condition trueBranch falseBranch
  trueBranch <- buildBranch "true" blockTrue resultPointer $ Just mainBr
  falseBranch <- buildBranch "false" blockFalse resultPointer $ Just mainBr
  mainBr <- block `named` bodyLabel
  result <- load resultPointer
  return result

emit (Int i) = pure (int32 i)

emit (Var v) = load (referenceIntPointer v)

emit (Def t name) = allocateDef (Def t name)

emit expr = error ("Impossible expression <" ++ show expr ++ ">")

__skip__ = "__skip__" -- hacky, but no so bad as you think, it's just a placeholder, not a flag

buildBranch name codeBlock resultPointer mNext =
  do
    branch <- block `named` name
    blockR <- buildCodeBlock codeBlock
    store resultPointer blockR
    case mNext of
      Nothing -> pure ()
      Just label -> br label
    return branch

allocArgs :: MonadIRBuilder m => [Expr] -> m ()
allocArgs (Def type_ name : exprs) = do
  p <- allocateInt `named` toShort' name
  store p (referenceInt $ argName name)
  allocArgs exprs
allocArgs [] = pure ()

buildCodeBlock exprBlock = do
  -- Steps of codegen
  extractDefs exprBlock
  ops <- mapM emit exprBlock
  return (last ops)

funcBodyBuilder :: (MonadFix m, MonadIRBuilder m) => [Expr] -> [Expr] -> ([Operand] -> m ())
funcBodyBuilder bodyTokens args = func
  where
    func argOperands = mdo
      block `named` bodyLabel
      allocArgs args   -- Dirty hack because I'm stupid and can't be bothered to make it use argOperands (which is the right way)
      result <- buildCodeBlock bodyTokens
      ret result
 
buildFunction func = case func of
    (Syntax.Function _ retType name args _ body) ->
      function (Name $ toShort' name) arguments (toLLVMType retType) funcBody
      where arguments = map argDef args
            funcBody = funcBodyBuilder body args

parseTopLevel (expr:exprs) = do
  case expr of
    (Syntax.Function md t n a r b) -> buildFunction (Syntax.Function md t n a r b) >> pure ()
    _ -> pure () -- TODO: return error
  parseTopLevel exprs
parseTopLevel [] = pure ()

buildIR :: [Expr] -> Module
buildIR exprs = buildModule "program" $ parseTopLevel exprs

{- GLOBAL TODOs (prioretized)
. fix operation priorities in parser
. sys calls (at least "write")
. unary operations
. fix how func arguments are processed
. support floats
. support string constants
. look for errors at AST level, before codegen
. switch, for, while and so on (at this stage we don't need those, but I obviously will have to implement them)
-}