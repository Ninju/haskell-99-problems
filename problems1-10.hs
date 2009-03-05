module Main where
import Data.List (foldl')

-- Solutions for Problems 1-10

-- Problem 1
 
lastElement :: [a] -> a
lastElement = head . reverse

-- Problem 2

secondLastElement :: [a] -> a
secondLastElement = head . tail . reverse

-- Problem 3

elementAt :: Int -> [a] -> a
elementAt n = head . drop (n-1)

-- Problem 4

listLength :: [a] -> Int
listLength = foldl' (\n _ -> 1 + n) 0

-- Problem 5

reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []

-- Problem 6

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 9

groupDuplicates :: Eq a => [a] -> [[a]]
groupDuplicates []     = []
groupDuplicates (x:xs) = (x:duplicates) : groupDuplicates rest
                         where
                         (duplicates, rest) = span (x==) xs

