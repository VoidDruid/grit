module Codegen.ASTBridge where

import Data.Map ((!))
import qualified Data.Map as Map

import LLVM.AST
import LLVM.AST.Type as AST

import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module

import Syntax
import Codegen.Primitives
import StringUtils

typeMap = Map.fromList [(IntType, i32)]

toLLVMType = (typeMap !)

argDef (Def defType name) = (typeMap ! defType, ParameterName $ toShort' (argName name))

allocateDef (Def defType name) = allocate (typeMap ! defType) `named` toShort' name
