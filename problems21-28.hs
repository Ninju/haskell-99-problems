module Main where
import System.Random
import Data.List

-- Problem 21

insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs | n < 1     = xs
                | otherwise = let (ys, zs) = splitAt (n - 1) xs
                              in ys ++ (x:zs)

-- Problem 22

range :: Int -> Int -> [Int]
range x y | x > y     = []
          | otherwise = x : range (x + 1) y


-- Problem 23

takeWhileAccum :: ([a] -> Bool) -> [a] -> [a]
takeWhileAccum f xs = 
  takeWhileAccum' f xs []
  where
  takeWhileAccum' f []     accum = accum
  takeWhileAccum' f (y:ys) accum = if f accum then takeWhileAccum' f ys (y:accum) else accum
              
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = 
  if length xs < n
  then return xs
  else
    do g <- newStdGen
       return $ map (xs !!) $ nub $ takeWhileAccum (\result -> length (nub result) < n) $ randomRs (0, length xs - 1) g

-- Problem 24

diff_select :: Int -> Int -> IO [Int]
diff_select n maxV = rnd_select [1..maxV] n

-- Problem 25

rnd_permu xs = return . concat =<< rnd_select (permutations xs) 1
