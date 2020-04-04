module Syntax where

--type Name = String
type CodeBlock = [Expr]

data ExprType = IntType | FloatType
  deriving (Eq, Ord, Show)

data Expr
  = Int Integer
  | Float Double
  | Var String
  | Def ExprType String
  | Call String [Expr]
  | Function ExprType String [Expr{-Should be Def-}] CodeBlock
  | BinaryOp String Expr Expr
  | UnaryOp String Expr
  | If Expr CodeBlock CodeBlock
  | Return Expr
  deriving (Eq, Ord, Show)
