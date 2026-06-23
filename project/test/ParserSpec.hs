module ParserSpec (spec) where

import Test.Hspec
import Parser  (parseString)
import AST

-- ============================================================
-- Helper: assert a parse succeeds and gives the expected AST
-- ============================================================

shouldParseAs :: String -> Program -> Expectation
input `shouldParseAs` expected =
    parseString input `shouldBe` Right expected

-- ============================================================
-- Helper: assert a parse fails (we don't care about the error
-- message, just that it is a Left)
-- ============================================================

shouldFail :: String -> Expectation
shouldFail input =
    case parseString input of
        Left  _   -> return ()   -- good, we expected failure
        Right ast -> expectationFailure $
            "Expected parse failure but got: " ++ show ast

-- ============================================================
-- The spec
-- ============================================================

spec :: Spec
spec = do

    -- ----------------------------------------------------------
    describe "Parser - variable declarations" $ do
    -- ----------------------------------------------------------

        it "parses an integer-range domain" $
            "var X : 1..3;\n"
                `shouldParseAs` Program
                    [VarDecl "X" (IntRange 1 3)]
                    []

        it "parses a discrete-set domain with integers" $
            "var C : {1, 2, 3};\n"
                `shouldParseAs` Program
                    [VarDecl "C" (DiscreteSet [IntVal 1, IntVal 2, IntVal 3])]
                    []

        it "parses a discrete-set domain with strings" $
            "var Color : {red, green, blue};\n"
                `shouldParseAs` Program
                    [VarDecl "Color" (DiscreteSet [StrVal "red", StrVal "green", StrVal "blue"])]
                    []

        it "parses multiple variable declarations" $
            "var A : 1..2;\nvar B : 1..2;\n"
                `shouldParseAs` Program
                    [ VarDecl "A" (IntRange 1 2)
                    , VarDecl "B" (IntRange 1 2)
                    ]
                    []

        it "skips single-line comments" $
            "// This is a comment\nvar X : 1..9;\n"
                `shouldParseAs` Program
                    [VarDecl "X" (IntRange 1 9)]
                    []


    -- ----------------------------------------------------------
    describe "Parser - constraints" $ do
    -- ----------------------------------------------------------

        it "parses a variable == literal constraint" $
            "var X : 1..5;\nconstraint X == 3;\n"
                `shouldParseAs` Program
                    [VarDecl "X" (IntRange 1 5)]
                    [Binary Eq (Var "X") (Lit (IntVal 3))]

        it "parses a variable /= variable constraint" $
            "var A : 1..3;\nvar B : 1..3;\nconstraint A /= B;\n"
                `shouldParseAs` Program
                    [VarDecl "A" (IntRange 1 3), VarDecl "B" (IntRange 1 3)]
                    [Binary NEq (Var "A") (Var "B")]

        it "parses a <= constraint" $
            "var A : 1..5;\nvar B : 1..5;\nconstraint A <= B;\n"
                `shouldParseAs` Program
                    [VarDecl "A" (IntRange 1 5), VarDecl "B" (IntRange 1 5)]
                    [Binary Le (Var "A") (Var "B")]

        it "parses an allDifferent constraint" $
            "var A : 1..3;\nvar B : 1..3;\nvar C : 1..3;\nconstraint allDifferent(A, B, C);\n"
                `shouldParseAs` Program
                    [ VarDecl "A" (IntRange 1 3)
                    , VarDecl "B" (IntRange 1 3)
                    , VarDecl "C" (IntRange 1 3)
                    ]
                    [NAry AllDifferent ["A", "B", "C"]]

        it "parses multiple constraints" $
            "var X : 1..3;\nvar Y : 1..3;\nconstraint X /= Y;\nconstraint X == 1;\n"
                `shouldParseAs` Program
                    [VarDecl "X" (IntRange 1 3), VarDecl "Y" (IntRange 1 3)]
                    [ Binary NEq (Var "X") (Var "Y")
                    , Binary Eq  (Var "X") (Lit (IntVal 1))
                    ]

        it "parses an empty program" $
            "" `shouldParseAs` Program [] []


    -- ----------------------------------------------------------
    describe "Parser - rejection of bad syntax" $ do
    -- ----------------------------------------------------------

        it "rejects a var declaration missing the colon" $
            shouldFail "var X 1..3;\n"

        it "rejects a var declaration missing the semicolon" $
            shouldFail "var X : 1..3\n"

        it "rejects a var declaration with no domain" $
            shouldFail "var X : ;\n"

        it "rejects a constraint with no right-hand side" $
            shouldFail "var X : 1..3;\nconstraint X == ;\n"

        it "rejects a bare constraint keyword with nothing after it" $
            shouldFail "constraint ;\n"

        it "rejects trailing garbage after a valid program" $
            shouldFail "var X : 1..3;\nGARBAGE\n"
