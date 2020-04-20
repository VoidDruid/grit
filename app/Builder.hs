{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Builder where

import Control.Applicative ((<$>))

import Data.Maybe
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Word (Word32)

import LLVM.AST hiding (function, alignment)
import LLVM.AST.AddrSpace
import LLVM.AST.Type as AST
import qualified LLVM.AST as A
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

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

integerPointer :: AST.Type
integerPointer = AST.PointerType i32 addrSpace

allocate :: MonadIRBuilder m => AST.Type -> m Operand
allocate type_ = alloca type_ Nothing alignment

allocateInt :: MonadIRBuilder m => m Operand
allocateInt = allocate i32

load :: MonadIRBuilder m => Operand -> m Operand
load pointer = I.load pointer alignment

store :: MonadIRBuilder m => Operand -> Operand -> m ()
store value pointer = I.store pointer alignment value

saveInt :: MonadIRBuilder m => Integer -> m Operand
saveInt value = do
  pointer <- allocateInt
  store (int32 value) pointer
  return pointer

refName :: String -> A.Name
refName name = Name (toShort' $  name ++ "_0")

referenceInt :: String -> Operand
referenceInt name = LocalReference i32 (refName name)

typeMap = Map.fromList [(IntType, i32)]

argDef (Def defType name) = (typeMap ! defType, ParameterName $ toShort' name)

-- TODO: extractDefs :: Expr -> Maybe Expr

funcBodyBuilder :: MonadIRBuilder m => [Expr] -> [Expr] -> ([Operand] -> m ())
funcBodyBuilder argTokens bodyTokens = func
  where func argOperands = do a <- named (saveInt 10) (toShort'"a")
                              b <- named (saveInt 20) (toShort' "b")
                              value <- add (referenceInt "a") (referenceInt "b")
                              ret value

functionAST (Syntax.Function retType name args body) = 
  function (Name $ toShort' name) arguments (typeMap ! retType) funcBody
  where arguments = map argDef args
        funcBody = funcBodyBuilder args body

buildAST :: [Expr] -> Module
buildAST [func] = buildModule "program" $ mdo functionAST func
