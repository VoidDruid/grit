{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.List (intersect)
import qualified Data.Text.Lazy.IO as TLIO

import System.IO
import System.Environment
import System.Exit
import System.Console.Haskeline

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.Pretty (ppllvm)

import JIT
import qualified Parser as P
import qualified AST.Processor as A
import qualified Codegen.Builder as B 

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      []      -> putStrLn "Provide file name!"
      (reverse -> (fname:args)) -> do
        code <- readFile fname
        case P.parseTopLevel code of
          Left err -> print err
          Right tokens -> do
            actionFor ["--debug", "-d"] (print tokens >> putStrLn "")
            let ast = A.processAST tokens
            actionFor ["--debug", "-d"] (print ast >> putStrLn "")
            let ir = B.buildIR ast
            actionFor ["--emit", "-e"] (TLIO.putStrLn $ ppllvm ir)
        return ()
        where
          actionFor key action = if not (null (key `intersect` args))
            then action
            else pure ()
