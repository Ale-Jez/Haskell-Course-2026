module SolverSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map.Strict as Map
import Data.List  (nub)

import AST
import Solver

-- Build an Assignment from a list of (name, int) pairs.
fromPairs :: [(String, Int)] -> Assignment
fromPairs = Map.fromList . map (\(k, v) -> (k, IntVal v))

-- The spec
spec :: Spec
spec = do

    describe "checkConstraint - binary Eq" $ do

        it "accepts X == 3 when X = 3" $
            checkConstraint (fromPairs [("X", 3)])
                (Binary Eq (Var "X") (Lit (IntVal 3)))
                `shouldBe` True

        it "rejects X == 3 when X = 5" $
            checkConstraint (fromPairs [("X", 5)])
                (Binary Eq (Var "X") (Lit (IntVal 3)))
                `shouldBe` False

        it "accepts 3 == 3 (two literals)" $
            checkConstraint Map.empty
                (Binary Eq (Lit (IntVal 3)) (Lit (IntVal 3)))
                `shouldBe` True

        it "rejects 1 == 2 (two literals)" $
            checkConstraint Map.empty
                (Binary Eq (Lit (IntVal 1)) (Lit (IntVal 2)))
                `shouldBe` False


    describe "checkConstraint - binary NEq / Lt / Le / Gt / Ge" $ do

        it "rejects A /= B when A = B = 1" $
            checkConstraint (fromPairs [("A", 1), ("B", 1)])
                (Binary NEq (Var "A") (Var "B"))
                `shouldBe` False

        it "accepts A /= B when A = 1, B = 2" $
            checkConstraint (fromPairs [("A", 1), ("B", 2)])
                (Binary NEq (Var "A") (Var "B"))
                `shouldBe` True

        it "accepts A < B when A = 1, B = 2" $
            checkConstraint (fromPairs [("A", 1), ("B", 2)])
                (Binary Lt (Var "A") (Var "B"))
                `shouldBe` True

        it "rejects A < B when A = 2, B = 1" $
            checkConstraint (fromPairs [("A", 2), ("B", 1)])
                (Binary Lt (Var "A") (Var "B"))
                `shouldBe` False

        it "accepts A >= B when A = 3, B = 3" $
            checkConstraint (fromPairs [("A", 3), ("B", 3)])
                (Binary Ge (Var "A") (Var "B"))
                `shouldBe` True


    describe "checkConstraint - partial assignment (unknown vars)" $ do

        it "returns True when the left variable is not yet assigned" $
            checkConstraint Map.empty
                (Binary Eq (Var "X") (Lit (IntVal 3)))
                `shouldBe` True

        it "returns True when both variables are not yet assigned" $
            checkConstraint Map.empty
                (Binary NEq (Var "A") (Var "B"))
                `shouldBe` True


    describe "checkConstraint - allDifferent" $ do

        it "accepts when all assigned values are distinct" $
            checkConstraint (fromPairs [("A", 1), ("B", 2), ("C", 3)])
                (NAry AllDifferent ["A", "B", "C"])
                `shouldBe` True

        it "rejects when two assigned values are equal" $
            checkConstraint (fromPairs [("A", 1), ("B", 1), ("C", 3)])
                (NAry AllDifferent ["A", "B", "C"])
                `shouldBe` False

        it "accepts when only some variables are assigned and they are distinct" $
            checkConstraint (fromPairs [("A", 1)])
                (NAry AllDifferent ["A", "B", "C"])
                `shouldBe` True

        it "accepts an empty allDifferent list" $
            checkConstraint Map.empty
                (NAry AllDifferent [])
                `shouldBe` True


    describe "domainValues" $ do

        it "IntRange 1 5 produces exactly 5 values" $
            length (domainValues (IntRange 1 5)) `shouldBe` 5

        it "IntRange 1 1 produces exactly 1 value" $
            domainValues (IntRange 1 1) `shouldBe` [IntVal 1]

        it "DiscreteSet returns the values in order" $
            domainValues (DiscreteSet [IntVal 3, IntVal 1, IntVal 2])
                `shouldBe` [IntVal 3, IntVal 1, IntVal 2]


    describe "QuickCheck - solver soundness" $ do

        it "every solution satisfies all constraints (soundness)" $
            property prop_soundness

        it "allDifferent solutions never contain duplicate values" $
            property prop_allDifferentSolutions

        it "applyBinOp Eq is symmetric" $
            property prop_eqSymmetric

        it "applyBinOp NEq is symmetric" $
            property prop_neqSymmetric

        it "domainValues (IntRange lo hi) has exactly (hi - lo + 1) elements" $
            property prop_intRangeSize




-- QuickCheck Generators
-- Generate a small variable name (single uppercase letter).
genVarName :: Gen String
genVarName = (:[]) <$> elements ['A'..'F']

-- Generate a small integer value
genSmallInt :: Gen Int
genSmallInt = choose (1, 3)

-- Generate a small IntRange domain.
genDomain :: Gen Domain
genDomain = IntRange <$> genSmallInt <*> pure 3

-- Generate a binary constraint between two (possibly equal) variable names.
genBinaryConstraint :: [String] -> Gen Constraint
genBinaryConstraint vars = do
    v1 <- elements vars
    v2 <- elements vars
    op <- elements [Eq, NEq, Lt, Le, Gt, Ge]
    return (Binary op (Var v1) (Var v2))

-- Generate CSP program
genSmallProgram :: Gen Program
genSmallProgram = do
    numVars <- choose (2, 3) :: Gen Int
    names   <- nub <$> vectorOf numVars genVarName
    let varDecls = map (\n -> VarDecl n (IntRange 1 3)) names
    numCons <- choose (1, 3) :: Gen Int
    cons    <- vectorOf numCons (genBinaryConstraint names)
    return (Program varDecls cons)



-- QuickCheck Properties
-- Every assignment returned by solve satisfies isValid.
prop_soundness :: Property
prop_soundness =
    forAll genSmallProgram $ \prog@(Program _ constraints) ->
        let solutions = solve prog
        in  all (\sol -> isValid sol constraints) solutions

-- For a pure allDifferent program, each solution has all distinct values.
prop_allDifferentSolutions :: Property
prop_allDifferentSolutions =
    forAll genAllDiffProgram $ \prog ->
        let solutions = solve prog
        in  all allDistinct solutions
  where
    genAllDiffProgram :: Gen Program
    genAllDiffProgram = do
        numVars <- choose (2, 3) :: Gen Int
        let names    = take numVars (map (:[]) ['A'..'Z'])
            varDecls = map (\n -> VarDecl n (IntRange 1 3)) names
            cons     = [NAry AllDifferent names]
        return (Program varDecls cons)

    allDistinct :: Assignment -> Bool
    allDistinct m =
        let vals = Map.elems m
        in  length vals == length (nub vals)

-- applyBinOp Eq is symmetric
prop_eqSymmetric :: Int -> Int -> Bool
prop_eqSymmetric a b =
    applyBinOp Eq (IntVal a) (IntVal b)
    ==
    applyBinOp Eq (IntVal b) (IntVal a)

-- applyBinOp NEq is symmetric.
prop_neqSymmetric :: Int -> Int -> Bool
prop_neqSymmetric a b =
    applyBinOp NEq (IntVal a) (IntVal b)
    ==
    applyBinOp NEq (IntVal b) (IntVal a)

-- IntRange lo hi always has exactly (hi - lo + 1) values.
prop_intRangeSize :: Property
prop_intRangeSize =
    forAll genBoundedRange $ \(lo, hi) ->
        length (domainValues (IntRange lo hi)) == hi - lo + 1
  where
    genBoundedRange :: Gen (Int, Int)
    genBoundedRange = do
        lo <- choose (1, 10)
        hi <- choose (lo, lo + 9)
        return (lo, hi)
