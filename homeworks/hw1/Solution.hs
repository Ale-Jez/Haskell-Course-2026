{-# LANGUAGE BangPatterns #-} 




-- Exercise 3: Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

-- Helper for Exercise 1 & 3
isPrime :: Int -> Bool
isPrime n 
    | n < 2     = False
    | otherwise = n `elem` primesTo n



-- Exercise 1: Goldbach Pairs
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = [(p, q) | p <- ps, q <- ps, p <= q, p + q == n]
  where ps = primesTo n



-- Exercise 2: Prime Factorization
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs = [(xs !! i, xs !! j) | i <- [0..length xs - 1], 
                                        j <- [i+1..length xs - 1], 
                                        gcd (xs !! i) (xs !! j) == 1]


-- Exercise 4: Matrix Multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b = [[sum [ (a !! i !! k) * (b !! k !! j) | k <- [0 .. p-1] ]
               | j <- [0 .. n-1]] 
               | i <- [0 .. m-1]]
  where
    m = length a
    p = length (head a)
    n = length (head b)



-- Exercise 5: Permutations of a list
permutations :: Int -> [a] -> [[a]]
permutations 0 _  = [[]]
permutations _ [] = []
permutations k xs = [x : ys | (x, rest) <- selections xs, ys <- permutations (k-1) rest]
  where
    -- Helper, pick one element and return the remaining list
    selections []     = []
    selections (y:ys) = (y, ys) : [(z, y:zs) | (z, zs) <- selections ys]




-- Exercise 6: Hamming numbers
-- (a) Merge two sorted lists without duplicates
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | x > y     = y : merge (x:xs) ys
    | otherwise = x : merge xs ys

-- (b) Infinite Hamming list
hamming :: [Integer]
hamming = 1 : merge (map (2*) hamming) (merge (map (3*) hamming) (map (5*) hamming))



-- Exercise 7: Recursive power function

power :: Int -> Int -> Int
power b e = helper 1 e
  where
    helper !acc 0 = acc
    helper !acc n = helper (acc * b) (n - 1)



-- Exercise 8: Finding the maximum element in a list
-- Version 1: using seq
listMaxSeq :: [Int] -> Int
listMaxSeq (x:xs) = go x xs
  where
    go acc [] = acc
    go acc (y:ys) = let next = max acc y in next `seq` go next ys

-- Version 2: using Bang Patterns
listMaxBang :: [Int] -> Int
listMaxBang (x:xs) = go x xs
  where
    go !acc [] = acc
    go !acc (y:ys) = go (max acc y) ys



-- Exercise 9: Infinite prime stream 
primes :: [Int]
primes = sieve [2..]

-- Improved isPrime using the infinite stream
isPrime' :: Int -> Bool
isPrime' n = n == head (dropWhile (< n) primes)



-- Exercise 10: Mean and Variance   
-- (a) No strictness (will cause space leak on large lists)
meanLazy :: [Double] -> Double
meanLazy xs = go 0 0 xs
  where
    go s n []     = s / n
    go s n (y:ys) = go (s + y) (n + 1) ys

-- (b) Fixed with Bang Patterns
-- Forcing the components (s and n) is necessary; 
-- a bang on a pair (e.g., !(s, n)) only forces the pair structure, not the numbers inside.
mean :: [Double] -> Double
mean xs = go 0 0 xs
  where
    go !s !n []     = s / n
    go !s !n (y:ys) = go (s + y) (n + 1) ys

-- (c) Mean and Variance in a single pass
-- Variance Formula: σ² = (Σx²) / n - μ²
meanVariance :: [Double] -> (Double, Double)
meanVariance xs = go 0 0 0 xs
  where
    go !sumX !sumX2 !n [] = 
        let mu = sumX / n
        in (mu, (sumX2 / n) - (mu * mu))
    go !sumX !sumX2 !n (y:ys) = 
        go (sumX + y) (sumX2 + (y * y)) (n + 1) ys