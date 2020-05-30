module Codegen.ASTBridge where

import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

import LLVM.AST hiding (VoidType)
import LLVM.AST.Type as AST hiding (VoidType)

import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module

import Syntax hiding (AST)
import Codegen.Primitives
import StringUtils

typeMap = Map.fromList 
  [ (IntType, i32)
  , (FloatType, double)
  , (VoidType, void)
  ]

toLLVMType = (typeMap !)

argDef (TypedExpr defType (TDef name)) = (toLLVMType defType, ParameterName $ toShort' (argName name))

allocateDef (TypedExpr defType (TDef name)) = allocate (toLLVMType defType) `named` toShort' name

allocateT :: MonadIRBuilder m => ExprType -> m Operand
allocateT type_ = allocate (toLLVMType type_)

pointerTo type_ = AST.PointerType (toLLVMType type_) addrSpace

referenceVar :: ExprType -> String -> Operand
referenceVar varType = reference (pointerTo varType)
