module Main where
import Prelude hiding (and, or)
import Data.List (intercalate)

-- Problem 46

and True True = True
and _    _    = False

or True _     = True
or _    True  = True
or _    _     = False

impl True  True = True
impl False _    = True
impl _     _    = False

xor True False = True
xor False True = True
xor _     _    = False

equ a b        = a == b

nand a b = not (and a b)
nor a b = not (or a b)

booleanCombos = [(True, True), (True, False), (False, True), (False, False)]
showBool True = "true"
showBool False = "fail"

table logicF = mapM (putStrLn . showTableRow . buildRow logicF) booleanCombos
               where
               buildRow f (a,b) = (a, b, f a b)
               showTableRow (a,b,c) = showBool a ++ " " ++ showBool b ++ " " ++ showBool c

-- Problem 48

choose :: [[a]] -> [[a]]
choose []       = []
choose (xs:[])  = map (:[]) xs
choose (xs:xss) =
  let restOfChoices = choose xss
  in
    do subChoice <- restOfChoices
       x         <- xs
       [x:subChoice]

copy n xs = map (const xs) [1..n]

generateCombinations :: Int -> [a] -> [[a]]
generateCombinations n values =
  let allChoices = copy n values
  in
    choose allChoices

tablen n logicF =
  mapM (putStrLn . showTableRow . buildRow logicF) (generateCombinations 3 [True, False])
  where
  buildRow f = \xs -> xs ++ [f xs]
  showTableRow xs = intercalate " " $ map showBool xs