{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}

module Builder where

import Control.Monad.Fix (MonadFix)
import Control.Applicative ((<$>))

import Data.Maybe
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Word (Word32)

import LLVM.AST hiding (function, alignment, Call)
import LLVM.AST.AddrSpace
import LLVM.AST.ParameterAttribute (ParameterAttribute)
import LLVM.AST.Type as AST
import qualified LLVM.AST as A
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IPredicats
import qualified LLVM.AST.FloatingPointPredicate as FPredicats

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction hiding (load, store)
import LLVM.IRBuilder.Constant
import qualified LLVM.IRBuilder.Instruction as I

import StringUtils
import Syntax

addrSpace :: AddrSpace
addrSpace = AddrSpace 0

iSize :: Word32
iSize = 32

alignment :: Word32
alignment = 4

integerConstant i = ConstantOperand (C.Int {C.integerBits = iSize, C.integerValue = i})

integerPointer :: AST.Type
integerPointer = AST.PointerType i32 addrSpace

allocate :: MonadIRBuilder m => AST.Type -> m Operand
allocate type_ = alloca type_ Nothing alignment

allocateInt :: MonadIRBuilder m => m Operand
allocateInt = allocate i32

load :: MonadIRBuilder m => Operand -> m Operand
load pointer = I.load pointer alignment

store :: MonadIRBuilder m => Operand -> Operand -> m ()
store pointer value = I.store pointer alignment value

saveInt :: MonadIRBuilder m => Integer -> m Operand
saveInt value = do
  pointer <- allocateInt
  store pointer (int32 value)
  return pointer

refName :: String -> A.Name
refName name = Name (toShort' $ name ++ "_0")

globalName :: String -> A.Name
globalName name = Name (toShort' name)

reference :: AST.Type -> String -> Operand
reference type_ name = LocalReference type_ (refName name)

referenceInt :: String -> Operand
referenceInt name = reference i32 name

referenceIntPointer :: String -> Operand
referenceIntPointer name = reference integerPointer name

typeMap = Map.fromList [(IntType, i32)]

bodyLabel = "Body"

argName = ("arg_" ++)

argDef (Def defType name) = (typeMap ! defType, ParameterName $ toShort' (argName name))

extractDefs :: MonadIRBuilder m => [Expr] -> m ()
extractDefs (expr:exprs) = do
  extractDef expr
  extractDefs exprs
extractDefs [] = pure ()

extractDef :: MonadIRBuilder m => Expr -> m ()
extractDef (BinaryOp "=" maybeDef _) = case maybeDef of
  Def defType defName -> do
    allocateInt `named` toShort' defName
    pure ()
  _ -> pure ()
extractDef _ = pure ()

emitAll :: (MonadFix m, MonadIRBuilder m) => [Expr] -> m ()
emitAll (expr:exprs) = do
  emit expr
  emitAll exprs
emitAll [expr] = emit expr

emit :: (MonadFix m, MonadIRBuilder m) => Expr -> m ()
emitInner :: MonadIRBuilder m => Expr -> m Operand
emitArgs :: MonadIRBuilder m => [Expr] -> m [(Operand, [ParameterAttribute])]

makeFuncRef :: String -> Operand
makeFuncRef funcName = ConstantOperand (C.GlobalReference funcType $ globalName funcName)
  where funcType = FunctionType i32 [] False

extractOperand :: MonadIRBuilder m => Expr -> m Operand
extractOperand expr = case expr of
  Int i -> pure (int32 i)
  Var v -> load (referenceIntPointer v)
  _ -> emitInner expr

emitArgs (expr:exprs) = do
  arg <- emitInner expr
  args <- emitArgs exprs
  return ((arg, []) : args)
emitArgs _ = return []

-- TODO: UnaryOp, inner if as expr? could be sweet
emitInner (BinaryOp operator opr1 opr2) = 
  do
    operand1 <- extractOperand opr1
    operand2 <- extractOperand opr2
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

emitInner (Call funcName exprs) =
  do
    args <- emitArgs exprs
    call (makeFuncRef funcName) args
  where
    emitArg expr = do
      op <- emitInner expr
      return (op, []) -- TODO

-- TODO: The following patterns can probably be simplified
emitInner (Int i) = extractOperand (Int i)
emitInner (Var v) = extractOperand (Var v)
emitInner _ = error "Impossible inner expression (error messages are WIP)"

emit (BinaryOp "=" dest object) = 
  do
    value <- extractOperand object
    store (referenceIntPointer name) value
  where
    name = case dest of 
      Def _ n -> n
      Var n -> n

emit (Return expr) = do
  value <- emitInner expr
  ret value

emit (Call f e) = emitInner (Call f e) >> pure ()

emit (If cond blockTrue blockFalse) = mdo
  condition <- emitInner cond
  condBr condition trueBranch falseBranch
  trueBranch <- buildBranch "true" blockTrue $ Just mainBr
  falseBranch <- buildBranch "false" blockFalse Nothing
  mainBr <- if isFinal blockTrue -- TODO: also if no more exprs after current If
    then pure __skip__
    else block `named` bodyLabel
  return ()

emit _ = pure ()

__skip__ = "__skip__" -- hacky, but no so bad as you think, it's just a placeholder, not a flag

isFinal = any (\case Return _ -> True; _ -> False)

buildBranch name codeBlock mNext =
  do
    branch <- block `named` name
    buildCodeBlock codeBlock
    finalInst
    return branch
  where 
    finalInst = case mNext of
      Nothing -> pure ()
      Just label -> if isFinal codeBlock
        then pure ()
        else br label

allocArgs :: MonadIRBuilder m => [Expr] -> m ()
allocArgs (Def type_ name : exprs) = do
  p <- allocateInt `named` toShort' name
  store p (referenceInt $ argName name)
  allocArgs exprs
allocArgs [] = pure ()

buildCodeBlock :: (MonadFix m, MonadIRBuilder m) => [Expr] -> m ()
buildCodeBlock exprBlock = do
  -- Steps of codegen
  extractDefs exprBlock
  emitAll exprBlock

funcBodyBuilder :: (MonadFix m, MonadIRBuilder m) => [Expr] -> [Expr] -> ([Operand] -> m ())
funcBodyBuilder bodyTokens args = func
  where
    func argOperands = do
      block `named` bodyLabel
      allocArgs args  -- Dirty hack because I'm stupid and can't be bothered to make it use argOperands (which is the right way)
      buildCodeBlock bodyTokens

functionAST (Syntax.Function modifiers retType name args body) = 
  function (Name $ toShort' name) arguments (typeMap ! retType) funcBody
  where arguments = map argDef args
        funcBody = funcBodyBuilder body args

parseTopLevel (expr:exprs) = do
  case expr of
    (Syntax.Function md r n a b) -> functionAST (Syntax.Function md r n a b) >> pure ()
    _ -> pure () -- TODO: return error
  parseTopLevel exprs
parseTopLevel [] = pure ()

buildAST :: [Expr] -> Module
buildAST exprs = buildModule "program" $ parseTopLevel exprs

{- GLOBAL TODOs (prioretized)
. fix operation priorities in parser
. sys calls (at least "write")
. unary operations
. fix how func arguments are processed
. support floats
. support string constants
. look for errors at AST level, before codegen
. commit to "everything is expression" and allow assigning ifs and so on?
. switch, for, while and so on (at this stage we don't need those, but I obviously will have to implement them)
-}