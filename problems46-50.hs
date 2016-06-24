module Main where
import Prelude hiding (and, or)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Graph.Inductive.Query.Monad (mapFst, mapSnd)
import Data.Tuple (swap)

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
-- choose all combinations of 1 from each sub array
-- choose [[a,b], [c,d]] == [[a,c], [a,d], [b,c], [b,d]]
choose :: [[a]] -> [[a]]
choose []       = []
choose (xs:[])  = map (:[]) xs
choose (xs:xss) =
  let restOfChoices = choose xss
  in
    do subChoice <- restOfChoices
       x         <- xs
       [x:subChoice]

generateCombinations :: Int -> [a] -> [[a]]
generateCombinations n values =
  let allChoices = take n $ cycle [values]
  in
    choose allChoices

tablen n logicF =
  mapM (putStrLn . showTableRow . buildRow logicF) (generateCombinations 3 [True, False])
  where
  buildRow f = \xs -> xs ++ [f xs]
  showTableRow xs = intercalate " " $ map showBool xs

-- Problem 49

gray n = generateCombinations n ['0', '1']

-- Problem 50

data HuffmanTree a = HuffmanTree Integer (HuffmanTree a) (HuffmanTree a) | HuffmanLeaf Integer a deriving Show

huffmanFreqAtNode (HuffmanLeaf freq _  ) = freq
huffmanFreqAtNode (HuffmanTree freq _ _) = freq

-- assumes initial list is already sorted by frequency
-- TODO: do a faster insert by using the fact that the list is sorted
constructHuffmanTree :: [HuffmanTree a] -> HuffmanTree a
constructHuffmanTree []               = error "Can not construct from empty list"
constructHuffmanTree [t]              = t
constructHuffmanTree (f:g:fs) =
  let newSubTree = HuffmanTree (huffmanFreqAtNode f + huffmanFreqAtNode g) f g
  in
    constructHuffmanTree $ sortBy (comparing huffmanFreqAtNode) (newSubTree:fs)

buildHuffmanTree :: [(Char, Integer)] -> HuffmanTree Char
buildHuffmanTree xs = constructHuffmanTree $ map (uncurry HuffmanLeaf . swap) $ sortBy (comparing snd) xs

buildHuffmanCodes tree currentSearchPath =
  case tree of
    HuffmanLeaf _ chr        -> (chr, currentSearchPath) : []
    HuffmanTree _ left right ->
      let leftCodes  = buildHuffmanCodes left  ('0':currentSearchPath)
          rightCodes = buildHuffmanCodes right ('1':currentSearchPath)
      in
        leftCodes ++ rightCodes

huffman :: [(Char, Integer)] -> [(Char, String)]
huffman xs =
  let tree = buildHuffmanTree xs
  in
    buildHuffmanCodes tree ""
