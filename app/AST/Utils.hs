module AST.Utils where

import Data.List
import Data.Maybe

import StringUtils
import Syntax

extractFuncRet :: Expr -> Maybe Expr
extractFuncRet (Syntax.Function md t n a r b) = case r of
  Nothing -> Nothing
  Just name -> Just $ Def t name


--- General purpose utilities

join = intercalate "\n"

prettyShow :: Int -> Expr -> String
prettyShow depth expr = concat $ replicate (depth * 2) " " ++ [exprRepr]
  where
    exprRepr = case expr of
      (Function m t n a r body) ->
        "Function " ++ show n ++ " " ++ show t ++ " ; args " ++ show a ++ " ; modifiers " ++ show m ++ " ; returns " ++ show r ++ "\n"
        ++ join (map (prettyShow (depth + 1)) body)
      _ -> show expr  -- TODO: pretty print for other exprs

prettyAST :: AST -> String
prettyAST ast = join $ map (prettyShow 0) ast

ppAST ast = putStrLn (prettyAST ast)
