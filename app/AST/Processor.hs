{-# LANGUAGE LambdaCase #-}

module AST.Processor where

import Control.Exception as E

import StringUtils
import Syntax

use [] x = x
use [f] x = f x
use (f:fs) x = use fs (f x)

processAST :: [Expr] -> [Expr]
processAST= use
  [ decorateAST
  ]

walkAST :: (Expr -> Expr) -> [Expr] -> [Expr]
process :: (Expr -> Expr) -> Expr  -> Expr

walkAST m = map (process m)
process m expr = m (process' m expr)

mapP m = map (process m)

process' m (Block codeBlock)= Block (walkAST m codeBlock)
process' m (Call name args) = Call name (mapP m args)
process' m (Function md t n args r body) = Function md t n (mapP m args) r (walkAST m body)
process' m (BinaryOp op e1 e2) = BinaryOp op (process m e1) (process m e2)
process' m (UnaryOp op e) = UnaryOp op (process m e)
process' m (If cond brT brF) = If (process m cond) (walkAST m brT) (walkAST m brF)
process' m DecoratorTarget = m DecoratorTarget
process' m e = e

-- TODO: can be optimized
decorateAST :: [Expr] -> [Expr]
decorateAST ast = 
    use (map (walkAST . decorateFuncAST) decorators) ast
    where decorators = extractDecorators ast

extractDecorators = filter (\case DecoratorDef{} -> True; _ -> False)

removeDecorator decName = filter (\case Decorator name -> name /= decName; _ -> True)

-- TODO: check for type errors and such
decorateFuncAST (DecoratorDef type_ name decBody) expr = case expr of
    Function mods t n a r body ->
        Function newMods t n a r newBody
        where
            (newBody, newMods) = if Decorator name `elem` mods
                then
                    (
                      walkAST (\e -> case e of
                        DecoratorTarget -> Block body;
                        _ -> e) decBody
                    , removeDecorator name mods
                    )
                else (body, mods)
    _ -> expr
