module Main where

-- Solutions for Problems 1-10


-- Problem 9

groupDuplicates :: Eq a => [a] -> [[a]]
groupDuplicates []     = []
groupDuplicates (x:xs) = (x:duplicates) : groupDuplicates rest
                         where
                         (duplicates, rest) = span (x==) xs

