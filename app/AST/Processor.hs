{-# LANGUAGE LambdaCase #-}

module AST.Processor where

import Control.Exception as E

import StringUtils
import Syntax

use [] x = x
use [f] x = f x
use (f:fs) x = use fs (f x)

-- TODO: checks, errors
-- TODO: optimizations?
processAST :: [Expr] -> [Expr]
processAST= use
  [ applyModifications
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

applyModifications :: [Expr] -> [Expr]
applyModifications ast = walkAST (
    use [ applyModificationsFunc decorators
        ]
    ) ast
    where decorators = filter (\case DecoratorDef{} -> True; _ -> False) ast 

findDecorator decName decorators =
    case filter (\case DecoratorDef _ name _ -> name == decName; _ -> False) decorators of
        [dec] -> dec
        -- multiple definition errors should be checked earlier

-- TODO: check for type errors and such?
applyModificationsFunc decorators expr = case expr of
    Function mods' _ _ _ _ _ ->
        use (map applyMod $ reverse mods') expr
        where
            applyMod m (Function mods t n a r body) = case m of
                Decorator decName ->
                    Function mods t n a r newBody
                    where newBody = decorateFuncAST (findDecorator decName decorators) body
    _ -> expr

decorateFuncAST (DecoratorDef _ _ decBody) body =
    walkAST (\e -> case e of DecoratorTarget -> Block body; _ -> e) decBody
