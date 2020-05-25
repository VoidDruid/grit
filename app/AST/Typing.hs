{-# LANGUAGE LambdaCase #-}

module AST.Typing where

import Data.Either
import Data.Map.Strict hiding (map)
import qualified Data.Map.Strict as Map

import StringUtils
import Syntax

newtype TypingError = TypingError String
  deriving Show

type TypeMap = Map Name ExprType

emptyTypeMap = Map.empty

-- TODO: this whole module should be rewritten with State monad

annotateTypes :: AST -> Either TAST TypingError
annotateTypes ast = case deduceBlock ast emptyTypeMap of
  Right e -> Right e
  Left (tast, _) -> Left tast

lookupType :: String -> TypeMap -> Either ExprType TypingError
lookupType name typeMap = case typeMap !? name of
  Just t -> Left t
  Nothing -> Right (TypingError $ "No definition for <" ++ name ++ "> could be found")

deduceType :: Expr -> TypeMap -> Either (TypedExpr, TypeMap) TypingError

deduceType (Block block) tm = 
  let tryTAST = deduceBlock block tm in
  case tryTAST of
    Right e -> Right e
    Left (tast, _) -> Left (TypedExpr blockType (TBlock tast), tm)
      where TypedExpr blockType _ = last tast

deduceType (Def type_ name) tm = Left
  ( TypedExpr type_ (TDef name)
  , insert name type_ tm )

deduceType (Int i) tm = Left (TypedExpr IntType (TInt i), tm)

deduceType (Float d) tm = Left (TypedExpr FloatType (TFloat d), tm)

deduceType (Var name) tm = 
  let typeLookup = lookupType name tm in
  case typeLookup of
    Right e -> Right e
    Left type_ -> Left (TypedExpr type_ (TVar name), tm)

deduceType (Call func args) tm =
  let typeLookup = lookupType func tm in
  case typeLookup of
    Right e -> Right e
    Left (CallableType _ type_) ->
      let tryTArgs = deduceBlock args tm in
      case tryTArgs of
        Right e -> Right e
        Left (tArgs, _) -> Left (TypedExpr type_ (TCall func tArgs), tm)

deduceType (Function _ retType name args _ body) tm = 
  let
    Left (tArgs, argsTm) = deduceBlock args tm
    funcType = CallableType (gatherTypes tArgs) retType
    funcTm = insert name funcType argsTm
    tryTBody = deduceBlock body funcTm
  in
    case tryTBody of
      Right e -> Right e
      Left (tBody, newTm) ->
        Left (TypedExpr funcType $ TFunction retType name tArgs tBody, newTm)

deduceType _ tm = Left (TypedExpr IntType (TInt 1), tm)  -- TODO: binary, unary, if, while

gatherTypes :: TAST -> [ExprType]
gatherTypes tBlock = map (\case TypedExpr type_ _ -> type_) tBlock

deduceBlock :: CodeBlock Expr -> TypeMap -> Either (TAST, TypeMap) TypingError

deduceBlock [] tm = Left ([], tm)

deduceBlock (expr:exprs) tm = case deduceType expr tm of
  Right e -> Right e
  Left (tExpr, newTm) -> case deduceBlock exprs newTm of
    Right e -> Right e
    Left (tast, newerTm) -> Left ((tExpr : tast), newerTm)
