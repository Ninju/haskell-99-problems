module Main where
import Data.List (find)

-- Problem 31

primes :: [Integer]
primes = sieve [2..]
         where
         sieve (p:xs) = p : sieve (filter (not . (== 0) . (`mod` p)) xs)

isPrime :: Integer -> Bool
isPrime n = case find (== n) $ takeWhile (<= n) primes of
              Nothing -> False
              Just _  -> True
