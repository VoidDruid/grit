{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import qualified Data.Text.Lazy.IO as TLIO

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.Pretty (ppllvm)

import JIT
import qualified Builder as B 
import qualified Parser as P

processFile fname = do
  code <- readFile fname
  let parseResult = P.parseTopLevel code
  let buildResult = case parseResult of
        Left err -> "Error"
        Right result -> ppllvm $ B.buildAST result
  putStrLn "TOKENS \n"
  print parseResult
  putStrLn "\nLLVM IR \n"
  TLIO.putStrLn buildResult

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> putStrLn "Provide file name!"
    [fname] -> processFile fname >> return ()
