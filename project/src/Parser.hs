module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import AST

-- Helpers

-- Skips spaces, tabs, newlines, and single-line comments (//)
whitespace :: Parser ()
whitespace = skipMany (skipSpace <|> skipComment)
  where
    skipSpace   = space >> return ()
    skipComment = try (string "//") >> many (noneOf "\n") >> return ()

-- A "lexeme" is a token that automatically consumes any trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = do
    result <- p
    whitespace
    return result

-- Parses a specific string
symbol :: String -> Parser String
symbol s = lexeme (string s)


-- Parsing Values

-- Parses an integer and wraps it in our IntVal constructor.
parseIntVal :: Parser Value
parseIntVal = do
    digits <- lexeme (many1 digit)
    return (IntVal (read digits))

-- Parses a string and wraps it in StrVal.
parseStrVal :: Parser Value
parseStrVal = do
    chars <- lexeme (many1 letter)
    return (StrVal chars)

-- | A Value is EITHER an IntVal OR a StrVal. 
parseValue :: Parser Value
parseValue = parseIntVal <|> parseStrVal



-- Parsing Domains

-- Parses a set
parseDiscreteSet :: Parser Domain
parseDiscreteSet = do
    symbol "{"
    -- sepBy automatically handles lists separated by a specific character
    vals <- parseValue `sepBy` symbol ","
    symbol "}"
    return (DiscreteSet vals)






-- Parses a variable name
identifier :: Parser String
identifier = lexeme $ do
    firstChar <- letter
    restChars <- many alphaNum
    return (firstChar : restChars)

-- Parses the binary operators. 
parseBinOp :: Parser BinOp
parseBinOp = 
      (try (symbol "==") >> return Eq)
  <|> (try (symbol "/=") >> return NEq)
  <|> (try (symbol "<=") >> return Le)
  <|> (try (symbol ">=") >> return Ge)
  <|> (symbol "<" >> return Lt)
  <|> (symbol ">" >> return Gt)


-- Parses an integer range
parseIntRange :: Parser Domain
parseIntRange = do
    low <- lexeme (many1 digit)
    symbol ".."
    high <- lexeme (many1 digit)
    return (IntRange (read low) (read high))


-- | A Domain is either a Discrete Set OR an Int Range.
parseDomain :: Parser Domain
parseDomain = parseDiscreteSet <|> parseIntRange







-- Parses an expression
parseExpr :: Parser Expr
parseExpr = try parseVarExpr <|> parseLitExpr
  where
    parseVarExpr = Var <$> identifier
    parseLitExpr = do
        digits <- lexeme (many1 digit)
        return (Lit (IntVal (read digits)))




-- Parses constraint 
parseBinaryConstraint :: Parser Constraint
parseBinaryConstraint = do
    symbol "constraint"
    expr1 <- parseExpr
    op    <- parseBinOp
    expr2 <- parseExpr
    symbol ";"
    return (Binary op expr1 expr2)

parseAllDifferentConstraint :: Parser Constraint
parseAllDifferentConstraint = do
    symbol "constraint"
    symbol "allDifferent"
    symbol "("
    vars <- identifier `sepBy` symbol ","
    symbol ")"
    symbol ";"
    return (NAry AllDifferent vars)


parseVarDecl :: Parser VarDecl
parseVarDecl = do
    symbol "var"
    name <- identifier
    symbol ":"
    domain <- parseDomain
    symbol ";"
    return (VarDecl name domain)

-- A constraint is EITHER a binary constraint OR an allDifferent constraint.
parseConstraint :: Parser Constraint
parseConstraint = try parseBinaryConstraint <|> parseAllDifferentConstraint







-- Parses the entire file
parseProgram :: Parser Program
parseProgram = do
    whitespace
    vars <- many parseVarDecl
    constraints <- many parseConstraint
    eof
    return (Program vars constraints)




-- Testing Helper

parseString :: String -> Either ParseError Program
parseString input = parse parseProgram "(unknown)" input


