module Syntax where

import Data.Maybe

type Name = String
type CodeBlock term = [Expr' term]

data ExprType
  = VoidType
  | IntType
  | FloatType
  | BytesType
  | CallableType [ExprType] ExprType
  | AutoType
  deriving (Eq, Ord, Show)

data Modifier
  = Decorator Name
  -- TODO
  deriving (Eq, Ord, Show)

data Expr' term
  = Int Integer
  | Float Double
  | Var Name
  | Def ExprType Name
  | DecoratorTarget
  | DecoratorDef ExprType Name (CodeBlock term)
  | Block (CodeBlock term)
  | Call String [Expr' term]
  | Function [Modifier] ExprType Name [Expr' term] (Maybe Name) (CodeBlock term)
  | BinaryOp String (Expr' term) (Expr' term)
  | UnaryOp String (Expr' term)
  | If (Expr' term) (CodeBlock term) (CodeBlock term)
  | While (Expr' term) (CodeBlock term)
  deriving (Eq, Ord, Show)

type Expr = Expr' ()
data TypedExpr = TypedExpr ExprType (Expr' TypedExpr)

type AST = [Expr]
type TAST = [TypedExpr]  -- Typed AST
