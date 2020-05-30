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
import AST.Utils (ppAST)
import AST.Errors (showE)
import qualified Parser as P
import qualified AST.Processor as A
import qualified Codegen.Builder as B 

import StringUtils

debugFlag = ["--debug", "-d"]
emitFlag = ["--emit", "-e"]

parseArgs :: [String] -> ([String], String) -- [flags], filename
parseArgs (reverse -> (filename:args)) = (args, filename) -- TODO: parse incoming arguments

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      []   -> putStrLn "Provide file name!"
      args -> do
        code <- readFile filename
        actionFor debugFlag (putStrLn $ filename ++ "\n")
        case P.parseCode code of
          Left err -> print err
          Right ast -> do
            actionFor debugFlag (ppAST ast >> putStrLn "")
            let maybeTAST = A.processAST ast
            case maybeTAST of
              Right errors -> putStrLn $ joinN (map showE errors)
              Left tast -> do
                actionFor debugFlag (ppAST tast >> putStrLn "")
                let ir = B.buildIR tast
                actionFor emitFlag (TLIO.putStrLn $ ppllvm ir)
        return ()
        where
          (flags, filename) = parseArgs args
          actionFor key action = if not (null (key `intersect` flags))
            then action
            else pure ()

{- GLOBAL TODOs (prioretized)
. look for errors at AST level - scopes info
. simple closures
. fix operation priorities in parser
. unary operations
. look for errors at AST level - type mismatch
. fix how func arguments are processed
. simple "auto" type
. support string constants
. do, for
. clean up all (most?) of the remaining TODOs, FIXMEs in code
. sys calls (at least "write") - some standart functions ("print", firstly)
. switch
-}
