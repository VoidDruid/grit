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

buildCodeBlock :: (MonadFix m, MonadIRBuilder m) => [TypedExpr] -> m Operand
emit :: (MonadFix m, MonadIRBuilder m) => TypedExpr -> m Operand

emit (view -> (_, TInt i)) = pure (int32 i)

emit (view -> (type_, TVar v)) = load (referenceVar type_ v)

emit def@(view -> (_, TDef _)) = allocateDef def

emit (view -> (_, TBlock codeBlock)) = buildCodeBlock codeBlock

emit (view -> (_, TBinaryOp "=" dest object)) =
  do
    value <- emit object
    (type_, name) <- getTypeName
    store (referenceVar type_ name) value
    return value -- Kinda like C++ '='
  where
    getTypeName = case view dest of 
      (t, TDef n) -> allocateDef dest >> return (t, n)
      (t, TVar n) -> return (t, n)

-- TODO: UnaryOp
emit (view -> (type_, TBinaryOp operator opr1 opr2)) = 
  do
    operand1 <- emit opr1
    operand2 <- emit opr2
    operation operand1 operand2
  where
    operation = case operator of  -- TODO: type checking
      "+" -> add
      "-" -> sub
      "*" -> mul
      --"/" -> div
      "<" -> icmp IPredicats.SLT
      ">" -> icmp IPredicats.SGT
      "==" -> icmp IPredicats.EQ
      "!=" -> icmp IPredicats.NE
      "<=" -> icmp IPredicats.SLE
      ">=" -> icmp IPredicats.SGE

emit (view -> (_, TCall funcName exprs)) =
  do
    args <- emitArgs exprs
    call (makeFuncRef funcName) args
  where
    emitArgs (e:es) = do
      arg <- emit e
      args <- emitArgs es
      return ((arg, []) : args)
    emitArgs _ = return []

emit (view -> (type_, TIf cond blockTrue blockFalse)) = mdo
  condition <- emit cond
  resultPointer <- allocateT type_ 
  condBr condition trueBranch falseBranch
  trueBranch <- buildBranch "true" blockTrue resultPointer $ Just mainBr
  falseBranch <- buildBranch "false" blockFalse resultPointer $ Just mainBr
  (mainBr, result) <- emitExit resultPointer
  return result

emit (view -> (type_, TWhile cond bodyBlock)) = mdo
  resultPointer <- allocateT type_
  br whileStart  -- we need terminator instruction at the end of the previous block, it will be optimized away
  whileStart <- block `named` "whileStart"
  condition <- emit cond
  condBr condition whileBody mainBr
  whileBody <- buildBranch "whileBody" bodyBlock resultPointer $ Just whileStart  -- after executing jump to beginning
  (mainBr, result) <- emitExit resultPointer
  return result

emit expr = error ("Impossible expression <" ++ show expr ++ ">")

buildBranch name codeBlock resultPointer mNext =
  do
    branch <- block `named` name
    blockR <- buildCodeBlock codeBlock
    store resultPointer blockR
    case mNext of
      Nothing -> pure ()
      Just label -> br label
    return branch

emitExit resultPointer = do
  mainBr <- block `named` bodyLabel
  result <- load resultPointer
  return (mainBr, result)

allocArgs :: MonadIRBuilder m => [TypedExpr] -> m ()
allocArgs ((TypedExpr type_ (TDef name)) : exprs) = do
  p <- (allocateT type_) `named` toShort' name
  store p (referenceVar type_ $ argName name)
  allocArgs exprs
allocArgs [] = pure ()

buildCodeBlock exprBlock = do
  -- Steps of codegen
  ops <- mapM emit exprBlock
  return (last ops)

funcBodyBuilder :: (MonadFix m, MonadIRBuilder m) => [TypedExpr] -> [TypedExpr] -> ([Operand] -> m ())
funcBodyBuilder bodyTokens args = func
  where
    func argOperands = mdo
      block `named` bodyLabel
      allocArgs args   -- Dirty hack because I'm stupid and can't be bothered to make it use argOperands (which is the right way)
      result <- buildCodeBlock bodyTokens
      ret result

buildFunction :: (MonadModuleBuilder m, MonadFix m) => ExprType -> TExpr -> m Operand
buildFunction (CallableType argsTypes retType) func@(TFunction name argsNames body) =
  function (Name $ toShort' name) arguments (toLLVMType retType) funcBody
  where typedArgs =  [TypedExpr t_ (TDef n) | t_ <- argsTypes, n <- argsNames]
        arguments = map argDef typedArgs
        funcBody = funcBodyBuilder body typedArgs

parseTopLevel ((TypedExpr t f):exprs) = do
  buildFunction t f >> pure ()
  parseTopLevel exprs
parseTopLevel [] = pure ()

buildIR :: TAST -> Module
buildIR exprs = buildModule "program" $ parseTopLevel exprs
