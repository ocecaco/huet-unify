module Main (main, solveFile) where

import Unify
import Parser
import qualified Data.Text.IO as TIO
import Syntax
import Control.Monad

solveFile :: String -> IO ()
solveFile filename = do
  contents <- TIO.readFile filename
  let Right parsed = parseProblem contents
  let solutions = runUnify (Just 1) (solveProblem parsed)
  case solutions of
    Left err -> print err
    Right sols -> forM_ sols $ \s -> do
      putStrLn "***** SOLUTION START ****"
      forM_ s $ \(m, t) -> do
        putStrLn "meta"
        print m
        print t
      putStrLn "***** SOLUTION END ****"

solveProblem :: ProblemDescription -> Unify [(MetaVar, Term)]
solveProblem desc = do
  converted <- liftTC $ convertProblem desc
  typeCheckThenUnify converted

main :: IO ()
main = putStrLn "Hello, Haskell!"
