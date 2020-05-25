module AST.Typing where

import Data.Either
import Data.Map.Strict
import qualified Data.Map.Strict as Map

import StringUtils
import Syntax

newtype TypingError = TypingError String

type TypeMap = Map Name ExprType

emptyTypeMap = Map.empty

lookupType :: String -> TypeMap -> Either ExprType TypingError
lookupType name typeMap = case typeMap !? name of
  Just t -> Left t
  Nothing -> Right (TypingError $ "No definition for <" ++ name ++ "> could be found")

deduceType :: Expr -> TypeMap -> Either (TypedExpr, TypeMap) TypingError

 -- TODO: can't use expr frm expr@<pattern> in TypedExpr definition, find fix

{-
deduceType (Block block) tm = 
  let tryTAST = annotateTypes block tm in
  case tryTAST of
    Right e -> Right e
    Left tast -> Left (TypedExpr blockType (Block tast), tm)
      where TypedExpr blockType _ = last tast
-}
deduceType (Def type_ name) tm = Left
  ( TypedExpr type_ (Def type_ name)
  , insert name type_ tm )

deduceType (Int i) tm = Left (TypedExpr IntType (Int i), tm)

deduceType (Float d) tm = Left (TypedExpr FloatType (Float d), tm)

deduceType (Var name) tm = 
  let typeLookup = lookupType name tm in
  case typeLookup of
    Right e -> Right e
    Left type_ -> Left (TypedExpr type_ (Var name), tm)

annotateTypes :: AST -> TypeMap -> Either TAST TypingError
annotateTypes ast tm = Left [TypedExpr IntType (Int 1)]  -- TODO: type annotating
