{-# LANGUAGE TypeSynonymInstances #-}

module Syntax where
import Debug.Trace

import Data.Maybe

import StringUtils

class Show e => Pretty e where  -- used below for Expr, TypedExpr and CodeBlock
  prettify :: e -> [String]

type Name = String
type CodeBlock term = [term]

data ExprType
  = VoidType
  | IntType
  | FloatType
  | BytesType
  | BooleanType
  | CallableType [ExprType] ExprType
  | AutoType
  deriving (Eq, Ord)

instance Show ExprType where
  show VoidType = "void"
  show IntType = "int"
  show FloatType = "float"
  show BytesType = "bytes"
  show BooleanType = "bool"
  show (CallableType from to) = "(" ++ fromRepr ++ " -> " ++ show to ++ ")"
    where
      fromRepr = case from of
        [] -> "()"
        from' -> joinC (map show from')
  show AutoType = "auto"

data Modifier
  = Decorator Name
  -- TODO
  deriving (Eq, Ord, Show)

data Expr
  = TypeCast ExprType Expr
  | Int Integer
  | Float Double
  | Var Name
  | Def ExprType Name
  | DecoratorTarget
  | DecoratorDef ExprType Name (CodeBlock Expr)
  | Block (CodeBlock Expr)
  | Call String [Expr]
  | Function [Modifier] ExprType Name [Expr] (Maybe Name) (CodeBlock Expr)
  | BinaryOp String Expr Expr
  | UnaryOp String Expr
  | If Expr (CodeBlock Expr) (CodeBlock Expr)
  | While Expr (CodeBlock Expr)
  deriving (Eq, Ord, Show)

data TExpr
  = TInt Integer
  | TFloat Double
  | TVar Name
  | TDef Name
  | TBlock (CodeBlock TypedExpr)
  | TCall String [TypedExpr]
  | TFunction Name [Name] (CodeBlock TypedExpr)
  | TBinaryOp String TypedExpr TypedExpr
  | TUnaryOp String TypedExpr
  | TIf TypedExpr (CodeBlock TypedExpr) (CodeBlock TypedExpr)
  | TWhile TypedExpr (CodeBlock TypedExpr)
  deriving (Eq, Ord, Show)

data TypedExpr = TypedExpr ExprType TExpr
  deriving (Eq, Ord, Show)

type AST = [Expr]
type TAST = [TypedExpr]  -- Typed AST

--- Utilities

joinOrSplit :: Pretty a => [String] -> a -> [String] 
joinOrSplit s e = case prettify e of
  [r] ->  addToLast s (" (" ++ r ++ ")")
  listR -> addToLast s " (" ++ ["  " ++ r' | r' <- listR] ++ [")"]

prettifyAST :: Pretty e => [e] -> [String]
prettifyAST = map (joinN . prettify)
joinedPrettyAST :: Pretty e => [e] -> String
joinedPrettyAST = joinN . prettifyAST

instance Pretty e => Pretty (CodeBlock e) where
  prettify exprs = concatMap tabExpr exprs
    where tabExpr e = map ("  " ++) (prettify e)

instance Pretty Expr where
  prettify expr = case expr of
    (TypeCast type_ e) -> joinOrSplit ["(" ++ show type_ ++ ")"] e
    (Int i) -> [joinS ["Int", show i]]
    (Float f) -> [joinS ["Float", show f]]
    (Var n) -> [joinS ["Var", show n]]
    (Def t n) -> [joinS ["Def", show t, show n]]
    (Block es) -> "Block {" : prettify es ++ ["}"]
    (Call f es) -> (joinS ["Call", show f, "("]) : prettify es ++ [")"]
    (Function m t n a r body) ->
        (joinS ["Function", show n, show t, "; args", show a, "; modifiers", show m, "; returns", show r, "{"])
        : prettify body ++ ["}"]
    (BinaryOp op e1 e2) -> joinOrSplit (joinOrSplit ["BinaryOp " ++ op] e1) e2
    (UnaryOp op e) -> joinOrSplit ["UnaryOp " ++ op] e
    (If eq bl1 bl2) -> addToLast (joinOrSplit ["If"] eq) " {" ++ prettify bl1 ++ ["}", "else {"] ++ prettify bl2 ++ ["}"]
    (While eq bl) -> addToLast (joinOrSplit ["While"] eq) " {" ++ prettify bl ++ ["}"]
    _ -> [show expr]  -- TODO: decorators

typedTuple (TypedExpr type_ tExpr) = (show type_, tExpr)
tT = typedTuple

instance Pretty TypedExpr where
  prettify expr = case expr of
    (tT -> (t, TInt i)) -> [joinS [t, show i]]
    (tT -> (t, TFloat f)) -> [joinS [t, show f]]
    (tT -> (t, TVar v)) -> [joinS ["Var", t, show v]]
    (tT -> (t, TDef d)) -> [joinS ["Def", t, show d]]
    (tT -> (t, TBlock es)) -> (joinS ["Block", t, "{"]) : prettify es ++ ["}"]
    (tT -> (t, TCall f es)) -> (joinS ["Call", t, show f, "("]) : prettify es ++ [")"]
    (tT -> (t, TFunction n argN body)) ->
      (joinS ["Function", t, n, show argN, "{"])
      : prettify body ++ ["}"]
    (tT -> (t, TBinaryOp op e1 e2)) -> joinOrSplit (joinOrSplit ["BinaryOp " ++ [head t] ++ op] e1) e2
    (tT -> (t, TUnaryOp op e)) -> joinOrSplit ["UnaryOp " ++ [head t] ++ op] e
    (tT -> (t, TIf eq bl1 bl2)) -> addToLast (joinOrSplit ["If " ++ t] eq) " {" ++ prettify bl1 ++ ["}", "else {"] ++ prettify bl2 ++ ["}"]
    (tT -> (t, TWhile eq bl)) -> addToLast (joinOrSplit ["While " ++ t] eq) " {" ++ prettify bl ++ ["}"]
