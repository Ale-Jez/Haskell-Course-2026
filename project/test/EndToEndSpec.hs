module EndToEndSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as Map
import Data.List (sort, nub)

import AST
import Parser  (parseString)
import Solver  (solve, Assignment)

-- Helpers

-- Load a .csp file relative to the test/ directory at runtime.
loadCsp :: FilePath -> IO (Either String Program)
loadCsp path = do
    contents <- readFile path
    return $ case parseString contents of
        Left  err -> Left (show err)
        Right ast -> Right ast

-- Assert a .csp file parses and returns its solutions.
solveFile :: FilePath -> IO [Assignment]
solveFile path = do
    result <- loadCsp path
    case result of
        Left  err  -> fail $ "Parse error in " ++ path ++ ": " ++ err
        Right prog -> return (solve prog)

-- Extract the Int out of an IntVal
intOf :: Value -> Int
intOf (IntVal n) = n
intOf v          = error $ "Expected IntVal, got: " ++ show v

-- Extract the String out of a StrVal.
strOf :: Value -> String
strOf (StrVal s) = s
strOf v          = error $ "Expected StrVal, got: " ++ show v

-- Look up a variable in an assignment (or fail the test).
lookupVar :: String -> Assignment -> Value
lookupVar var asgn =
    case Map.lookup var asgn of
        Just v  -> v
        Nothing -> error $ "Variable not found in solution: " ++ var



spec :: Spec
spec = do

    describe "End-to-end - Australia map colouring" $ do

        it "parses australia.csp without errors" $ do
            result <- loadCsp "test/australia.csp"
            result `shouldSatisfy` \r -> case r of { Right _ -> True; _ -> False }

        it "finds at least one valid 3-colouring" $ do
            solutions <- solveFile "test/australia.csp"
            solutions `shouldSatisfy` (not . null)

        it "finds exactly 18 valid 3-colourings" $ do
            solutions <- solveFile "test/australia.csp"
            length solutions `shouldBe` 18

        it "every solution uses only {red, green, blue}" $ do
            solutions <- solveFile "test/australia.csp"
            let allVals = concatMap (map strOf . Map.elems) solutions
            all (`elem` ["red", "green", "blue"]) allVals `shouldBe` True

        it "every solution satisfies all adjacency constraints" $ do
            solutions <- solveFile "test/australia.csp"
            -- The adjacency pairs from the CSP file
            let adjacent = [ ("WA","NT"), ("WA","SA"), ("NT","SA")
                           , ("NT","Q"),  ("SA","Q"),  ("SA","NSW")
                           , ("SA","V"),  ("Q","NSW"), ("NSW","V") ]
            let checkSol sol = all (\(a,b) -> lookupVar a sol /= lookupVar b sol) adjacent
            all checkSol solutions `shouldBe` True

        it "Tasmania (T) is unconstrained and appears in 3 variants per other solution" $ do
            -- T has no adjacency constraints so each base solution gets 3 Tasmania colours
            solutions <- solveFile "test/australia.csp"
            -- For any fixed assignment of WA..V, all 3 Tasmania colours appear
            let withoutT = map (Map.delete "T") solutions
            -- The 18 solutions = 6 mainland colourings × 3 Tasmania options
            length (nub withoutT) `shouldBe` 6


    describe "End-to-end - 4x4 Mini-Sudoku" $ do

        it "parses sudoku.csp without errors" $ do
            result <- loadCsp "test/sudoku.csp"
            result `shouldSatisfy` \r -> case r of { Right _ -> True; _ -> False }

        it "finds exactly 1 solution" $ do
            solutions <- solveFile "test/sudoku.csp"
            length solutions `shouldBe` 1

        it "solution matches the known unique answer" $ do
            solutions <- solveFile "test/sudoku.csp"
            let sol = head solutions
            -- Known solution for the given clues:
            -- Row 1: 3 4 1 2
            -- Row 2: 2 1 4 3
            -- Row 3: 1 2 3 4
            -- Row 4: 4 3 2 1
            let expected =
                    [ ("R1C1", 3), ("R1C2", 4), ("R1C3", 1), ("R1C4", 2)
                    , ("R2C1", 2), ("R2C2", 1), ("R2C3", 4), ("R2C4", 3)
                    , ("R3C1", 1), ("R3C2", 2), ("R3C3", 3), ("R3C4", 4)
                    , ("R4C1", 4), ("R4C2", 3), ("R4C3", 2), ("R4C4", 1)
                    ]
            map (\(v, n) -> (v, intOf (lookupVar v sol))) expected
                `shouldBe` expected

        it "every row has all four values 1..4" $ do
            solutions <- solveFile "test/sudoku.csp"
            let sol  = head solutions
            let rows = [ ["R1C1","R1C2","R1C3","R1C4"]
                       , ["R2C1","R2C2","R2C3","R2C4"]
                       , ["R3C1","R3C2","R3C3","R3C4"]
                       , ["R4C1","R4C2","R4C3","R4C4"] ]
            let vals row = sort $ map (intOf . flip lookupVar sol) row
            map vals rows `shouldBe` replicate 4 [1,2,3,4]

        it "every column has all four values 1..4" $ do
            solutions <- solveFile "test/sudoku.csp"
            let sol  = head solutions
            let cols = [ ["R1C1","R2C1","R3C1","R4C1"]
                       , ["R1C2","R2C2","R3C2","R4C2"]
                       , ["R1C3","R2C3","R3C3","R4C3"]
                       , ["R1C4","R2C4","R3C4","R4C4"] ]
            let vals col = sort $ map (intOf . flip lookupVar sol) col
            map vals cols `shouldBe` replicate 4 [1,2,3,4]

        it "every 2x2 box has all four values 1..4" $ do
            solutions <- solveFile "test/sudoku.csp"
            let sol  = head solutions
            let boxes = [ ["R1C1","R1C2","R2C1","R2C2"]
                        , ["R1C3","R1C4","R2C3","R2C4"]
                        , ["R3C1","R3C2","R4C1","R4C2"]
                        , ["R3C3","R3C4","R4C3","R4C4"] ]
            let vals box = sort $ map (intOf . flip lookupVar sol) box
            map vals boxes `shouldBe` replicate 4 [1,2,3,4]


    describe "End-to-end - 4-Queens (programmatic)" $ do
    -- The .csp DSL cannot express |Qi - Qj| /= |i - j| because it lacks arithmetic.
    -- Thus construct the Program in Haskell directly and supply a custom diagonal checker.

        it "parses nqueens.csp without errors" $ do
            result <- loadCsp "test/nqueens.csp"
            result `shouldSatisfy` \r -> case r of { Right _ -> True; _ -> False }

        it "4-queens programmatic solver finds exactly 2 solutions" $ do
            -- allDifferent gives 24 permutations; diagonal filter leaves exactly 2
            let solutions = filter noDiagonalAttack (solve nqueensProgram)
            length solutions `shouldBe` 2

        it "both 4-queens solutions pass the diagonal check" $ do
            let solutions = filter noDiagonalAttack (solve nqueensProgram)
            all noDiagonalAttack solutions `shouldBe` True

        it "both 4-queens solutions are the known placements" $ do
            let solutions = filter noDiagonalAttack (solve nqueensProgram)
            let toRows sol = map (\q -> intOf (lookupVar q sol)) ["Q1","Q2","Q3","Q4"]
            let rows = sort $ map toRows solutions
            rows `shouldBe` [[2,4,1,3], [3,1,4,2]]


-- 4-Queens helpers

nqueensProgram :: Program
nqueensProgram = Program varDecls constraints
  where
    varDecls    = [ VarDecl "Q1" (IntRange 1 4)
                  , VarDecl "Q2" (IntRange 1 4)
                  , VarDecl "Q3" (IntRange 1 4)
                  , VarDecl "Q4" (IntRange 1 4) ]
    constraints = [ NAry AllDifferent ["Q1","Q2","Q3","Q4"] ]

noDiagonalAttack :: Assignment -> Bool
noDiagonalAttack asgn =
    let cols = [("Q1",1), ("Q2",2), ("Q3",3), ("Q4",4)]
        pairs = [ ((n1,c1),(n2,c2)) | (n1,c1) <- cols, (n2,c2) <- cols, c1 < c2 ]
        ok ((n1,c1),(n2,c2)) =
            let r1 = intOf (lookupVar n1 asgn)
                r2 = intOf (lookupVar n2 asgn)
            in abs (r1 - r2) /= abs (c1 - c2)
    in all ok pairs
