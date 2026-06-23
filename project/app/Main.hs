module Main where

import System.Environment (getArgs)
import Parser (parseString)
import Solver (solve)
import qualified Data.Map as Map

-- | The entry point of the executable
main :: IO ()
main = do
    putStrLn "=== CSP Solver Engine ==="
    
    -- Get command line arguments
    args <- getArgs
    
    case args of
        [filename] -> processFile filename
        _          -> putStrLn "Usage: cabal run project-exe -- <file.csp>"

-- | Reads a file, parses it, and solves it.
processFile :: FilePath -> IO ()
processFile filename = do
    putStrLn $ "Reading file: " ++ filename
    contents <- readFile filename
    
    -- Step 1: Parse
    case parseString contents of
        Left err -> do
            putStrLn "\n[ERROR] Failed to parse file:"
            print err
            
        Right ast -> do
            putStrLn "[SUCCESS] File parsed perfectly."
            putStrLn "Searching for solutions...\n"
            
            -- Step 2: Solve
            let solutions = solve ast
            
            -- Step 3: Output Results
            if null solutions
                then putStrLn "Result: UNSATISFIABLE (No solutions exist)."
                else do
                    putStrLn $ "Found " ++ show (length solutions) ++ " valid solution(s)!"
                    -- Print the very first solution we found cleanly
                    let firstSolution = head solutions
                    mapM_ (\(var, val) -> putStrLn $ "  " ++ var ++ " = " ++ show val) (Map.toList firstSolution)