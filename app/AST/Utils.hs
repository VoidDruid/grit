module AST.Utils where

import Data.Maybe

import StringUtils
import Syntax

extractFuncRet :: Expr -> Maybe Expr
extractFuncRet (Syntax.Function md t n a r b) = case r of
  Nothing -> Nothing
  Just name -> Just $ Def t name
