module Syntax where

import Data.Maybe

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
  deriving (Eq, Ord, Show)

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
