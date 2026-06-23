module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import AST

-- ==========================================
-- 1. Lexer Helpers (Handling the boring stuff)
-- ==========================================

-- | Skips spaces, tabs, newlines, AND single-line comments (//)
whitespace :: Parser ()
whitespace = skipMany (skipSpace <|> skipComment)
  where
    skipSpace   = space >> return ()
    skipComment = try (string "//") >> many (noneOf "\n") >> return ()

-- | A "lexeme" is a token that automatically consumes any trailing whitespace.
-- This saves us from having to manually check for spaces everywhere.
lexeme :: Parser a -> Parser a
lexeme p = do
    result <- p
    whitespace
    return result

-- | Parses a specific string (like "var" or "{") and swallows following spaces.
symbol :: String -> Parser String
symbol s = lexeme (string s)

-- ==========================================
-- 2. Parsing Small Pieces (Values)
-- ==========================================

-- | Parses an integer and wraps it in our IntVal constructor.
parseIntVal :: Parser Value
parseIntVal = do
    digits <- lexeme (many1 digit) -- many1 means "at least one"
    return (IntVal (read digits))  -- 'read' turns the string into an actual Int

-- | Parses a string (like "red" or "blue") and wraps it in StrVal.
parseStrVal :: Parser Value
parseStrVal = do
    chars <- lexeme (many1 letter)
    return (StrVal chars)

-- | A Value is EITHER an IntVal OR a StrVal. 
-- The `<|>` operator is the magic "try this, if it fails, try that" operator.
parseValue :: Parser Value
parseValue = parseIntVal <|> parseStrVal

-- ==========================================
-- 3. Parsing Domains
-- ==========================================

-- | Parses a set like: { red, green, blue }
parseDiscreteSet :: Parser Domain
parseDiscreteSet = do
    symbol "{"
    -- sepBy automatically handles lists separated by a specific character!
    vals <- parseValue `sepBy` symbol ","
    symbol "}"
    return (DiscreteSet vals)






    -- | Parses a variable name (just a string of letters for now).
-- | Parses a variable name (starts with a letter, followed by letters/numbers).
identifier :: Parser String
identifier = lexeme $ do
    firstChar <- letter
    restChars <- many alphaNum
    return (firstChar : restChars)


    

-- | Parses the binary operators. 
-- Order matters here! We must check for "<=" before "<", otherwise 
-- the parser will consume the "<", see the "=", and crash.
parseBinOp :: Parser BinOp
parseBinOp = 
      (try (symbol "==") >> return Eq)
  <|> (try (symbol "/=") >> return NEq)
  <|> (try (symbol "<=") >> return Le)
  <|> (try (symbol ">=") >> return Ge)
  <|> (symbol "<" >> return Lt)
  <|> (symbol ">" >> return Gt)


-- | Parses an integer range like: 1..9
parseIntRange :: Parser Domain
parseIntRange = do
    low <- lexeme (many1 digit)
    symbol ".."
    high <- lexeme (many1 digit)
    return (IntRange (read low) (read high))


-- | A Domain is either a Discrete Set OR an Int Range.
-- | A general domain parser. Right now it just calls parseDiscreteSet, 
-- but this makes it easy to add 'parseIntRange' later using <|>.
parseDomain :: Parser Domain
parseDomain = parseDiscreteSet <|> parseIntRange







-- | Parses an expression: either a variable name or an integer literal.
parseExpr :: Parser Expr
parseExpr = try parseVarExpr <|> parseLitExpr
  where
    parseVarExpr = Var <$> identifier
    parseLitExpr = do
        digits <- lexeme (many1 digit)
        return (Lit (IntVal (read digits)))

-- | Parses: constraint WA /= NT;  OR  constraint R1C1 == 3;
parseBinaryConstraint :: Parser Constraint
parseBinaryConstraint = do
    symbol "constraint"
    expr1 <- parseExpr
    op    <- parseBinOp
    expr2 <- parseExpr
    symbol ";"
    return (Binary op expr1 expr2)

-- | Parses: constraint allDifferent(A, B, C);
parseAllDifferentConstraint :: Parser Constraint
parseAllDifferentConstraint = do
    symbol "constraint"
    symbol "allDifferent"
    symbol "("
    vars <- identifier `sepBy` symbol ","
    symbol ")"
    symbol ";"
    return (NAry AllDifferent vars)


-- | Parses: var WA : { red, green, blue };
parseVarDecl :: Parser VarDecl
parseVarDecl = do
    symbol "var"
    name <- identifier
    symbol ":"
    domain <- parseDomain
    symbol ";"
    return (VarDecl name domain)

-- -- | Parses: constraint WA /= NT;
-- parseConstraint :: Parser Constraint
-- parseConstraint = do
--     symbol "constraint"
--     var1 <- identifier
--     op <- parseBinOp
--     var2 <- identifier
--     symbol ";"
--     return (Binary op var1 var2)

-- | A constraint is EITHER a binary constraint OR an allDifferent constraint.
-- We use 'try' so if it reads "constraint" but it's not binary, it rewinds and checks allDifferent.
parseConstraint :: Parser Constraint
parseConstraint = try parseBinaryConstraint <|> parseAllDifferentConstraint







    -- | Parses the entire file.
parseProgram :: Parser Program
parseProgram = do
    whitespace                 -- Clear any whitespace at the very top of the file
    vars <- many parseVarDecl  -- 'many' means "zero or more"
    constraints <- many parseConstraint
    eof                        -- End Of File: ensures we didn't leave unparsed garbage at the bottom
    return (Program vars constraints)

-- ==========================================
-- 4. Testing Helper
-- ==========================================

-- | A convenience function to run our parser on a string and print the result.
-- You can use this in GHCi to test your code.
parseString :: String -> Either ParseError Program
parseString input = parse parseProgram "(unknown)" input


