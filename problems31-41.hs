module Main where
import Data.List (find, foldr)
import Problems1to10 (encode)

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

totient :: Integer -> Int
totient n = length $ filter (coprime n) [1..(n - 1)]

-- Problem 35

primeFactors :: Integer -> [Integer]
primeFactors n = primeFactors' n primes
                 where
                 primeFactors' n ps | n < 2 = []
                                    | otherwise = let (p:ps) = dropWhile (not . (0 ==) . mod n) primes 
                                                  in p : primeFactors' (div n p) (p:ps)

-- Problem 36

primeFactorsMulti :: Integer -> [(Integer, Int)]
primeFactorsMulti = map swap . encode . primeFactors
                    where
                    swap (x, y) = (y, x)

-- Problem 37

totient' :: Integer -> Integer
totient' = foldr bam 1 . primeFactorsMulti
           where
           bam (p, m) acc = acc * (p - 1) * p ^ (m - 1)

-- Problem 38

-- Not a coding problem

-- Problem 39

primesR :: Integer -> Integer -> [Integer]
primesR a b = dropWhile (< a) . takeWhile (<= b) $ primes

-- Problem 40

maybeGoldbach :: Integer -> Maybe (Integer, Integer)
maybeGoldbach n =
  let primes' = takeWhile (< n) primes
  in
    findPair primes'
  where
  findPair [] = Nothing
  findPair (p:ps) = 
    case find ((n - p) ==) (p:ps) >>= \v -> Just (p, v) of
      Nothing -> findPair ps
      result -> result

goldbach :: Integer -> (Integer, Integer)
goldbach n =
  case maybeGoldbach n of
    Nothing -> error "Must be true for all small even n"
    Just (a,b) -> (a,b)

-- Problem 41

andP f g (x,y) = f x && g y

goldbachList :: Integer -> Integer -> [(Integer, Integer)]
goldbachList lower upper 
  | (lower `mod` 2) /= 0 = goldbachList (lower + 1) upper
  | lower > upper = []
  | otherwise = goldbach lower : goldbachList (lower + 2) upper

goldbachList' lower upper lowerPrime =
  let biggerThanLowerPrime = (>= lowerPrime)
  in
    filter (andP biggerThanLowerPrime biggerThanLowerPrime) $ goldbachList lower upper
