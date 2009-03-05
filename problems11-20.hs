module Main where
import Data.List (group)

-- Problem 11

data Element a = Element a | EncodedElement Int a deriving Show

encode' :: Eq a => [a] -> [Element a]
encode' = map encodeElem' . group
          where
          encodeElem' ys = case ys of
                            [y] -> Element y
                            _   -> EncodedElement (length ys) (head ys)
