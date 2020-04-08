{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Builder where

import Control.Applicative ((<$>))

import Data.Maybe
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
import Syntax

integerConstant i = ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = i})

typeMap = Map.fromList [(IntType, i32)]

argDef (Def defType name) = (typeMap ! defType, ParameterName $ toShort' name)

-- TODO: extractDefs :: Expr -> Maybe Expr

funcBodyBuilder :: MonadIRBuilder m => [Expr] -> [Expr] -> ([Operand] -> m ())
funcBodyBuilder argTokens bodyTokens = func
  where func [] = do ret $ integerConstant 1

functionAST (Syntax.Function retType name args body) = 
  function (Name $ toShort' name) arguments (typeMap ! retType) funcBody
  where arguments = map argDef args
        funcBody = funcBodyBuilder args body

buildAST :: [Expr] -> Module
buildAST [func] = buildModule "program" $ mdo functionAST func
