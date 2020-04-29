module Syntax where

type Name = String
type CodeBlock = [Expr]

data ExprType = IntType | FloatType
  deriving (Eq, Ord, Show)

data Modifier
  = Decorator Name
  -- TODO
  deriving (Eq, Ord, Show)

data Expr
  = Int Integer
  | Float Double
  | Var Name
  | Def ExprType Name
  | Call String [Expr]
  | DecoratorDef ExprType Name CodeBlock
  | DecoratorTarget
  | Function [Modifier] ExprType Name [Expr] CodeBlock
  | BinaryOp String Expr Expr
  | UnaryOp String Expr
  | If Expr CodeBlock CodeBlock
  | Return Expr
  deriving (Eq, Ord, Show)
