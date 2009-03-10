module Main where

-- Problem 21

insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs | n < 1     = xs
                | otherwise = let (ys, zs) = splitAt (n - 1) xs
                              in ys ++ (x:zs)
