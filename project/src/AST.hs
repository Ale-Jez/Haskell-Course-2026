module AST where

-- A complete CSP program consists of variable declarations and constraints.
data Program = Program [VarDecl] [Constraint]
  deriving (Show, Eq)

-- A variable declaration gives a name (String) and defines its initial domain.
data VarDecl = VarDecl String Domain
  deriving (Show, Eq)

-- The domain of allowed values for a variable.
data Domain
  = IntRange Int Int
  | DiscreteSet [Value]
  deriving (Show, Eq)

-- Values variables can take.
data Value
  = IntVal Int
  | StrVal String
  | BoolVal Bool
  deriving (Show, Eq, Ord) 

-- An expression in a constraint: either a variable name or a literal value.
data Expr
  = Var String
  | Lit Value
  deriving (Show, Eq)

-- Constraints define the rules between variables (referenced by their string names).
data Constraint
  = Binary BinOp Expr Expr
  | NAry NAryOp [String]
  deriving (Show, Eq)

-- Binary operators for comparing two variables.
data BinOp
  = Eq  -- Equals
  | NEq -- Not Equals
  | Lt  -- Less than
  | Le  -- Less than or equal
  | Gt  -- Greater than
  | Ge  -- Greater than or equal
  deriving (Show, Eq)

-- N-ary operators applied to a list of variables.
data NAryOp
  = AllDifferent
  deriving (Show, Eq)