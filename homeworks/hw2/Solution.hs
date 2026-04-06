module Solution where

import Data.Foldable (toList)

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving (Show, Eq)




-- Task 1: Functor for Sequence

instance Functor Sequence where
    -- fmap :: (a -> b) -> Sequence a -> Sequence b
    fmap _ Empty        = Empty
    fmap f (Single x)   = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)





-- Task 2: Foldable for Sequence

instance Foldable Sequence where
    -- foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap _ Empty        = mempty
    foldMap f (Single x)   = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r


seqToList :: Sequence a -> [a]
seqToList = toList 

seqLength :: Sequence a -> Int
seqLength = length






-- Task 3: Semigroup and Monoid for Sequence

instance Semigroup (Sequence a) where
    Empty <> seq = seq
    seq <> Empty = seq
    seq1 <> seq2 = Append seq1 seq2

instance Monoid (Sequence a) where
    mempty = Empty


-- Task 4: Tail Recursion and Sequence Search

tailElem :: Eq a => a -> Sequence a -> Bool
tailElem target initialSeq = go [initialSeq]
  where
    go [] = False
    go (Empty : rest) = go rest
    go (Single x : rest)
        | x == target = True
        | otherwise   = go rest
    go (Append l r : rest) = go (l : r : rest)






-- Task 5: Tail Recursion and Sequence Flatten

tailToList :: Sequence a -> [a]
tailToList initialSeq = go [initialSeq] []
  where
    go [] acc = acc
    go (Empty : rest) acc = go rest acc
    go (Single x : rest) acc = go rest (x : acc)
    
    go (Append l r : rest) acc = go (r : l : rest) acc






-- Task 6: Tail Recursion and Reverse Polish Notation

data Token = TNum Int | TAdd | TSub | TMul | TDiv 
    deriving (Show, Eq)

tailRPN :: [Token] -> Maybe Int
tailRPN initialTokens = go initialTokens []
  where
    go [] [result] = Just result
    go [] _ = Nothing

    go (TNum n : rest) stack = go rest (n : stack)
    
    go (TAdd : rest) (y : x : stack) = go rest ((x + y) : stack)
    go (TSub : rest) (y : x : stack) = go rest ((x - y) : stack)
    go (TMul : rest) (y : x : stack) = go rest ((x * y) : stack)
    
    go (TDiv : rest) (0 : x : stack) = Nothing 
    go (TDiv : rest) (y : x : stack) = go rest ((x `div` y) : stack)
    
    go _ _ = Nothing






-- Task 7: Expressing functions via foldr and foldl

-- (a) myReverse using foldl
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

-- (b) myTakeWhile using foldr
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

-- (c) decimal using folds
decimal :: [Int] -> Int
decimal = foldl (\acc x -> acc * 10 + x) 0






-- Task 8: Run-length encoding via folds

-- (a) encode using foldr
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x [] = [(x, 1)]
    step x acc@((y, count) : rest)
        | x == y    = (y, count + 1) : rest
        | otherwise = (x, 1) : acc

-- (b) decode using foldr
decode :: [(a, Int)] -> [a]
decode = foldr (\(x, count) acc -> replicate count x ++ acc) []