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
import qualified Builder as B 
import qualified Parser as P

processFile fname = do
  code <- readFile fname
  let parseResult = P.parseTopLevel code
  let buildResult = case parseResult of
        Left err -> Nothing
        Right result -> Just $ B.buildAST result
  return (parseResult, buildResult)

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      []      -> putStrLn "Provide file name!"
      (reverse -> (fname:args)) -> do
        (mTokens, mIR) <- processFile fname
        case mTokens of
          Left err -> print err
          Right tokens -> actionFor ["--debug", "-d"] (print tokens >> putStrLn "")
        case mIR of
          Nothing -> pure ()
          Just ir -> actionFor ["--emit", "-e"] (TLIO.putStrLn $ ppllvm ir)
        return ()
        where
          actionFor key action = if not (null (key `intersect` args))
            then action
            else pure ()
