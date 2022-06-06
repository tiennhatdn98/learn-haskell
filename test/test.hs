import System.IO

-- Quick Sort
sort []     = []
sort (x:xs) = sort ys ++ [x] ++ sort zs where
  ys = [a | a <- xs, a <= x]
  zs = [b | b <- xs, b > x]

double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns
add' :: Int -> (Int -> Int)
add' x y = x + y

-- Define function
abs1 :: Int -> Int
abs1 n = if n >= 0 then n else -n

-- Guarded Equation
abs2 :: Int -> Int
abs2 n  | n >= 0 = n
        | otherwise = -n

-- Pattern Matching (Patterns are matched in order, patterns may not repeat variables)
and' :: Bool  -> Bool -> Bool
and' True b   = b
and' False _  = False
-- and' b b = b -> Gives an error

-- Functions on lists can be defined using x:xs patterns.
head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' [] = []

-- Find prime numbers from n to m
-- Find divisors of number n
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
-- Prime number has 2 divisors included 1 and itself
prime :: Int -> Bool
prime n = factors n == [1, n]
-- Find prime numbers from a to b
primes :: Int -> Int -> [Int]
primes a b = [x | x <- [a..b], prime x]
-- Eratosthenes Sieve
eratosthenes :: [Int] -> [Int]
eratosthenes (p:xs) = p : eratosthenes [x | x <- xs, x `mod` p /= 0]
eratosthenes [] = []

allPrimes :: [Int]
allPrimes = eratosthenes [2..]

-- Check list is sorted
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]