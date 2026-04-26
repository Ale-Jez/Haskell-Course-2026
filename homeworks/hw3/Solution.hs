import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (permutations)
import Control.Monad (guard) 
import Control.Monad.Writer



type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)



-- 1(a)
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = Map.lookup pos maze >>= Map.lookup dir

-- 1(b)
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _ pos [] = Just pos
followPath maze pos (d:ds) = do
    nextPos <- move maze pos d
    followPath maze nextPos ds

-- 1(c)
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath _ pos [] = Just [pos]
safePath maze pos (d:ds) = do
    nextPos <- move maze pos d
    restOfPath <- safePath maze nextPos ds
    return (pos : restOfPath)





-- A simple 2x2 L-shaped maze for testing
-- (0,1) <-> (1,1)
--             |
-- (0,0) <-> (1,0)
testMaze :: Maze
testMaze = Map.fromList
    [ ((0,0), Map.fromList [(E, (1,0))])
    , ((1,0), Map.fromList [(W, (0,0)), (N, (1,1))])
    , ((1,1), Map.fromList [(S, (1,0)), (W, (0,1))])
    , ((0,1), Map.fromList [(E, (1,1))])
    ]





-- 2

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key str = traverse (\c -> Map.lookup c key) str

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key wordsList = traverse (decrypt key) wordsList



testKey :: Key
testKey = Map.fromList [('a', 'z'), ('b', 'y'), ('c', 'x'), ('d', 'w')]



-- 3
type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    perm <- permutations guests
    
    let pairs = case perm of
            [] -> []
            xs -> zip xs (tail xs ++ [head xs])
            
    let isConflict (g1, g2) = (g1, g2) `elem` conflicts || (g2, g1) `elem` conflicts
    guard (not (any isConflict pairs))
    return perm


testGuests :: [Guest]
testGuests = ["Alice", "Bob", "Charlie", "Dave"]

-- Alice and Bob cannot sit together. Charlie and Dave cannot sit together.
testConflicts :: [Conflict]
testConflicts = [("Alice", "Bob"), ("Charlie", "Dave")]




-- 4(a)
data Result a = Failure String | Success a [String] 
  deriving (Show, Eq)

instance Functor Result where
    fmap _ (Failure msg) = Failure msg
    fmap f (Success val warnings) = Success (f val) warnings

instance Applicative Result where
    pure val = Success val [] 
    
    Failure msg <*> _ = Failure msg
    Success _ _ <*> Failure msg = Failure msg
    Success f w1 <*> Success val w2 = Success (f val) (w1 ++ w2) 

instance Monad Result where
    Failure msg >>= _ = Failure msg
    Success val w1 >>= f = case f val of
        Failure msg -> Failure msg
        Success newVal w2 -> Success newVal (w1 ++ w2) 


-- 4(b)
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure msg = Failure msg


-- 4(c)
validateAge :: Int -> Result Int
validateAge age
    | age < 0   = failure "Age cannot be negative"
    | age > 150 = do
        warn ("Age " ++ show age ++ " is unusually high")
        return age
    | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges ages = mapM validateAge ages




-- 5
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr 
  deriving (Show, Eq)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)
simplify (Neg e) = do
    e' <- simplify e
    simplifyNeg e'
simplify (Add e1 e2) = do
    e1' <- simplify e1
    e2' <- simplify e2
    simplifyAdd e1' e2'
simplify (Mul e1 e2) = do
    e1' <- simplify e1
    e2' <- simplify e2
    simplifyMul e1' e2'

-- Helpers
simplifyNeg :: Expr -> Writer [String] Expr
simplifyNeg (Neg e) = do
    tell ["Double negation: Neg (Neg e) -> e"]
    return e
simplifyNeg e = return (Neg e)

simplifyAdd :: Expr -> Expr -> Writer [String] Expr
simplifyAdd (Lit 0) e = do
    tell ["Add identity: 0 + e -> e"]
    return e
simplifyAdd e (Lit 0) = do
    tell ["Add identity: e + 0 -> e"]
    return e
simplifyAdd (Lit a) (Lit b) = do
    tell ["Constant folding: " ++ show a ++ " + " ++ show b ++ " -> " ++ show (a + b)]
    return (Lit (a + b))
simplifyAdd e1 e2 = return (Add e1 e2)

simplifyMul :: Expr -> Expr -> Writer [String] Expr
simplifyMul (Lit 0) _ = do
    tell ["Zero absorption: 0 * e -> 0"]
    return (Lit 0)
simplifyMul _ (Lit 0) = do
    tell ["Zero absorption: e * 0 -> 0"]
    return (Lit 0)
simplifyMul (Lit 1) e = do
    tell ["Mul identity: 1 * e -> e"]
    return e
simplifyMul e (Lit 1) = do
    tell ["Mul identity: e * 1 -> e"]
    return e
simplifyMul (Lit a) (Lit b) = do
    tell ["Constant folding: " ++ show a ++ " * " ++ show b ++ " -> " ++ show (a * b)]
    return (Lit (a * b))
simplifyMul e1 e2 = return (Mul e1 e2)


-- 6
newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show, Eq)

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    
    ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)


-- 6(c) Why ZipList cannot be a Monad
{-
ZipList can't be a Monad because writing bind (>>=) for it just doesn't work.

for Applicative we had to make `pure` create an infinite list. but monads have the law where `return x >>= f` has to be exactly `f x`.

so if `f` returns a super short list like [1, 2], `return x >>= f` is supposed to just be [1, 2]. 
but `return x` is infinite
so we are dumping an infinite amount of x's into `f`. 

this gives us back an infinite number of lists of any length. 
and how to combine them? 
taking respective items from each list is one way but but what if one list only has 1 item? 
trying to grab the nth item from it would go out of bounds and crash. 

because the lengths are all over the place, there's no safe way to line them up, so the monad law falls apart.
-}