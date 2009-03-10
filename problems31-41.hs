module Main where
import Data.List (find)

-- Problem 31

primes :: [Integer]
primes = sieve [2..]
         where
         sieve (p:xs) = p : sieve (filter (not . (== 0) . (`mod` p)) xs)

isPrime :: Integer -> Bool
isPrime n = elem n $ takeWhile (<= n) primes

-- Problem 32

gcd' :: Integer -> Integer -> Integer
gcd' a 0 = abs a
gcd' a b = gcd' b (a `mod` b)

-- Problem 33

coprime :: Integer -> Integer -> Bool
coprime a b = gcd a b == 1

-- Problem 34

totient :: Integer -> Integer
totient n = length $ filter (coprime n) [1..(n - 1)]
