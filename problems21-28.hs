module Main where
import System.Random
import Data.List hiding(group)
import Data.Graph.Inductive.Query.Monad

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

rnd_permu :: [a] -> IO [a]
rnd_permu xs = return . concat =<< rnd_select (permutations xs) 1

-- Problem 26

combinations :: Int -> [a] -> [[a]]
combinations 0 xs     = []
combinations 1 xs     = map (:[]) xs
combinations n []     = []
combinations n (x:xs) = (map (x:) $ combinations (n - 1) xs) ++ combinations n xs

-- Problem 27

chooseAndSplit :: Int -> [a] -> [([a], [a])]
chooseAndSplit 0 xs     = [([], xs)]
chooseAndSplit k []     = [([], [])]
chooseAndSplit k (x:xs) =
  let choicesWithX    = map (mapFst (x:)) $ chooseAndSplit (k - 1) xs
  in
    if k <= length xs
    then
      let choicesWithoutX = map (mapSnd (x:)) $ chooseAndSplit k xs
      in
        choicesWithoutX ++ choicesWithX
    else choicesWithX

group :: [Int] -> [a] -> [[[a]]]
group []             _      = []
group groupSizes     []     = []
group (n:[])         people = map ((:[]) . fst) (chooseAndSplit n people)
group (n:groupSizes) people =
  let firstGroupChoices = chooseAndSplit n people
  in
    foldr
      (\(firstGroupChoice, others) res ->
         res ++ map (firstGroupChoice:) (group groupSizes others))
      []
      firstGroupChoices
