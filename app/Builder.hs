{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Builder where

import Control.Applicative ((<$>))

import Data.Map ((!))
import qualified Data.Map as Map

import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import StringUtils
import qualified Syntax as S

integerConstant i = ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = i})

typeMap = Map.fromList [(S.IntType, i32)]

definitionAST (S.Def defType name) = (typeMap ! defType, ParameterName $ toShort' name)

functionAST (S.Function retType name args body) = 
  function (Name $ toShort' name) arguments (typeMap ! retType) funcBody
  where arguments = map definitionAST args
        funcBody [] = mdo
          do ret $ integerConstant 1

buildAST :: [S.Expr] -> Module
buildAST [func] = buildModule "program" $ mdo functionAST func
