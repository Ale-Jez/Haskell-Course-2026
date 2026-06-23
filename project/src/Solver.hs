module Solver where

import AST
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (nub)

-- ==========================================
-- 1. State Representation
-- ==========================================

-- | An Assignment is a dictionary mapping a variable's name to its current guessed Value.
type Assignment = Map String Value

-- ==========================================
-- 2. Checking Constraints
-- ==========================================

-- | Applies a binary operator to two values.
-- We rely on the fact that we derived 'Eq' and 'Ord' in AST.hs!
applyBinOp :: BinOp -> Value -> Value -> Bool
applyBinOp Eq  val1 val2 = val1 == val2
applyBinOp NEq val1 val2 = val1 /= val2
applyBinOp Lt  val1 val2 = val1 <  val2
applyBinOp Le  val1 val2 = val1 <= val2
applyBinOp Gt  val1 val2 = val1 >  val2
applyBinOp Ge  val1 val2 = val1 >= val2

-- | Evaluates an Expr to a Maybe Value given the current assignment.
-- A Lit is always known; a Var is only known if it's been assigned.
evalExpr :: Assignment -> Expr -> Maybe Value
evalExpr _          (Lit v)    = Just v
evalExpr assignment (Var name) = Map.lookup name assignment

-- | Checks if a single constraint is satisfied given our current partial assignment.
-- If a variable in the constraint hasn't been assigned yet, we return True 
-- (meaning "it's not violated yet, keep going").
checkConstraint :: Assignment -> Constraint -> Bool
checkConstraint assignment (Binary op e1 e2) =
    case (evalExpr assignment e1, evalExpr assignment e2) of
        (Just val1, Just val2) -> applyBinOp op val1 val2
        _                      -> True -- One or both sides are unassigned

checkConstraint assignment (NAry AllDifferent vars) =
    -- Extract only the values that have currently been assigned
    let assignedVals = [val | v <- vars, Just val <- [Map.lookup v assignment]]
    -- 'nub' removes duplicates. If the length changes, there was a duplicate!
    in length assignedVals == length (nub assignedVals)

-- | Checks if ALL constraints in the program are satisfied by the current assignment.
isValid :: Assignment -> [Constraint] -> Bool
isValid assignment constraints = 
    all (checkConstraint assignment) constraints





-- ==========================================
-- 3. Domain Helpers
-- ==========================================

-- | Converts our Domain data type into a simple list of possible Values.
domainValues :: Domain -> [Value]
domainValues (DiscreteSet vals) = vals
domainValues (IntRange low high) = map IntVal [low .. high]

-- ==========================================
-- 4. The Backtracking Search
-- ==========================================

-- | Takes a Program and returns a list of ALL valid assignments (solutions).
-- If the list is empty, the problem is unsatisfiable.
solve :: Program -> [Assignment]
solve (Program unassignedVars constraints) = backtrack Map.empty unassignedVars
  where
    -- 'backtrack' takes the current guesses and the remaining variables to guess.
    backtrack :: Assignment -> [VarDecl] -> [Assignment]
    
    -- Base Case: If there are no variables left to assign, we found a solution!
    backtrack assignment [] = [assignment]
    
    -- Recursive Case: Pick the next variable and try guessing its value.
    backtrack assignment (VarDecl name domain : rest) = do
        
        -- 1. Try EVERY value in this variable's domain.
        -- Because we are in the List monad, Haskell will automatically fork 
        -- the execution path for every single value in this list!
        val <- domainValues domain
        
        -- 2. Create a new dictionary with our new guess added.
        let newAssignment = Map.insert name val assignment
        
        -- 3. The Pruning Step: Check if this guess broke any rules.
        if isValid newAssignment constraints
            then backtrack newAssignment rest -- It's valid! Keep digging down this branch.
            else []                           -- It broke a rule! Return an empty list (kill this branch).