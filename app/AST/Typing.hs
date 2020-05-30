{-# LANGUAGE LambdaCase #-}

module AST.Typing where

import Data.Either
import Data.Map.Strict hiding (map, foldl)
import qualified Data.Map.Strict as Map

import AST.Errors
import StringUtils
import Syntax

type TypeMap = Map Name ExprType

emptyTypeMap = Map.empty

-- TODO: this whole module should be rewritten with State monad
-- TODO: maybe do something smarter with binary and unary operations
-- TODO: return [GTypeError] instead of single GTypeError
-- TODO: BinaryOperations float checks?

annotateTypes :: AST -> Either TAST [GTypeError]
annotateTypes ast = case deduceBlock ast emptyTypeMap of
  Right e -> Right [e] -- TODO: type list from deduceType
  Left (tast, _) -> Left tast

lookupType :: String -> TypeMap -> Either ExprType GTypeError
lookupType name typeMap = case typeMap !? name of
  Just t -> Left t
  Nothing -> Right (GTypeError $ "No definition for <" ++ name ++ "> could be found")

deduceType :: Expr -> TypeMap -> Either (TypedExpr, TypeMap) GTypeError

deduceType (Block block) tm = 
  let tryTAST = deduceBlock block tm in
  case tryTAST of
    Right e -> Right e
    Left (tast, _) -> Left (TypedExpr (getLastType tast) (TBlock tast), tm)

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
    argsNames = map (\case Def _ n -> n) args
    Left (tArgs, argsTm) = deduceBlock args tm
    funcType = CallableType (gatherTypes tArgs) retType
    funcTm = insert name funcType argsTm
    tryTBody = deduceBlock body funcTm
  in
    case tryTBody of
      Right e -> Right e
      Left (tBody, newTm) ->
        Left (TypedExpr funcType $ TFunction name argsNames tBody, newTm)

deduceType (UnaryOp op expr) tm = 
  let tryTExpr = deduceType expr tm in
  case tryTExpr of
    Right e -> Right e
    Left (tExpr@(TypedExpr type_ _), _) ->
      Left (TypedExpr type_ $ TUnaryOp op tExpr, tm)

deduceType (BinaryOp op expr1 expr2) tm =
  let tryOps = deduceBlock [expr1, expr2] tm in
  case tryOps of
    Right e -> Right e
    Left (ops@[op1, op2], newTm) -> Left (result, newTm)
      where
        buildT finType = TypedExpr finType (TBinaryOp op op1 op2)
        leftwiseCast = buildT (getHeadType ops)
        result
          | op == "=" = leftwiseCast
          | op == "/" = buildT FloatType
          | op `elem` [">", "<", ">=", "<=", "==", "!="] = buildT BooleanType
          | op `elem` ["%", "//"] = buildT IntType
          | otherwise = leftwiseCast

deduceType (While eq body) tm =
  let
    tryEq = deduceType eq tm
    tryBody = deduceBlock body tm
  in
    -- NOTE: I know its not pretty, but I cant think of something better yet
    case tryEq of
      Right e -> Right e
      Left (tEq, _) ->
        case tryBody of
          Right e -> Right e
          Left (tBody, _) ->
            Left (TypedExpr (getLastType tBody) (TWhile tEq tBody), tm)

-- Check both branches have equal types
deduceType (If eq br1 br2) tm = 
  let
    tryEq = deduceType eq tm
    tryBlocks = gatherBlocks [br1, br2] tm
  in
    -- NOTE: I know its not pretty, but I cant think of something better yet
    case tryEq of
      Right e -> Right e
      Left (tEq, _) ->
        case tryBlocks of
          Right e -> Right e
          Left ([tBr1, tBr2], _) ->
            Left (TypedExpr (getLastType tBr1) (TIf tEq tBr1 tBr2), tm)

deduceType (TypeCast type_ expr) tm =
  let tryExpr = deduceType expr tm in
  case tryExpr of
    Right e -> Right e
    Left (TypedExpr _ tExpr, newTm) -> Left (TypedExpr type_ tExpr, newTm)

foldBlocks :: Either ([TAST], TypeMap) GTypeError -> AST -> Either ([TAST], TypeMap) GTypeError
foldBlocks curBlocks nextBlock = case curBlocks of
  Right e -> Right e
  Left (eBlocks, tm) ->
    let tryBlock = deduceBlock nextBlock tm in
    case tryBlock of
      Right e -> Right e
      Left (tBlock, _) -> Left (eBlocks ++ [tBlock], tm)

gatherBlocks :: [AST] -> TypeMap -> Either ([TAST], TypeMap) GTypeError
gatherBlocks ast tm = foldl foldBlocks (Left ([], tm)) ast

gatherTypes :: TAST -> [ExprType]
gatherTypes = map (\case TypedExpr type_ _ -> type_)

getTypeFrom getter tast = case getter tast of
  TypedExpr type_ _ -> type_

getHeadType = getTypeFrom head
getLastType = getTypeFrom last

deduceBlock :: CodeBlock Expr -> TypeMap -> Either (TAST, TypeMap) GTypeError

deduceBlock [] tm = Left ([], tm)

deduceBlock (expr:exprs) tm = case deduceType expr tm of
  Right e -> Right e
  Left (tExpr, newTm) -> case deduceBlock exprs newTm of
    Right e -> Right e
    Left (tast, newerTm) -> Left (tExpr : tast, newerTm)
