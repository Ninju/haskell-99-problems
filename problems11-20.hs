module Main where
import Data.List (group)

-- Problem 11

data Element a = Element a | EncodedElement Int a deriving Show

encode' :: Eq a => [a] -> [Element a]
encode' = map encodeElem' . group
          where
          encodeElem' xs = case xs of
                           [x] -> Element x
                           _  -> EncodedElement (length xs) (head xs)

-- Problem 12

decode :: [Element a] -> [a]
decode = concatMap decodeElem
         where
         decodeElem (Element e)          = [e]
         decodeElem (EncodedElement n e) = replicate n e


-- Problem 13
encodeDirect xs = 
  fst $
    foldl 
      (\(r, xs') e -> 
        let (ds,ts) = (dropWhile (== e) xs', takeWhile (== e) xs')
        in
          case ts of
            []  -> (r,ds)
            [e] -> (r ++ [Element e], ds)
            ts' -> (r ++ [EncodedElement (length ts') e], ds))
      ([], xs)
      xs

        

          
      

-- Problem 14

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-- Problem 15

repli :: Int -> [a] -> [a]
repli = concatMap . replicate

-- Problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n - 1) xs ++ dropEvery (drop n xs) n

-- Problem 17

split :: [a] -> Int -> ([a],[a])
split = flip splitAt

-- Problem 18

slice :: [a] -> Int -> Int -> [a]
slice xs u v = take (v - u + 1) $ drop (u - 1) xs

-- Problem 19

rotate :: [a] -> Int -> [a]
rotate xs n = take (length xs) $ drop n $ cycle xs

-- Problem 20

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n xs | n < 1     = (Nothing, xs)
              | otherwise = removeAt' n xs
              where
              removeAt' _ []     = (Nothing, [])
              removeAt' 1 (x:xs) = (Just x, xs)
              removeAt' n (x:xs) = let (y,ys) = removeAt' (n - 1) xs
                                   in (y, x:ys)
