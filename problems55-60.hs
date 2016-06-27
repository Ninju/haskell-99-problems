module Main where

-- so we can use the drawTree function
import Data.Tree hiding (Tree)

toDataTree Empty = Node "nil" []
toDataTree (Branch k l r) = Node (k:[]) [toDataTree l, toDataTree r]
-- END compatibility with Data.Tree

-- From definition in https://wiki.haskell.org/99_questions/54A_to_60
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf x = Branch x Empty Empty
-- END definitions from https://wiki.haskell.org/99_questions/54A_to_60

data TreePath = LeftBranch | RightBranch deriving Show

treePathInsert tree path x =
  case (tree, path) of
    (Empty,           _) -> leaf x
    (Branch x l r, path) ->
      case path of
        []                  -> Branch x (treePathInsert l [] x) r
        (LeftBranch:path')  -> Branch x (treePathInsert l path' x) r
        (RightBranch:path') -> Branch x l (treePathInsert r path' x)

-- Problem 55

nChoices 1 values = map (:[]) values
nChoices n values =
  do v <- values
     map (v:) (nChoices (n - 1) values)

cbalTree n =
  let insertPaths = take n $ foldr (\n combos -> nChoices n [LeftBranch, RightBranch] ++ combos) [] [1..]
  in
    foldl (\tree path -> treePathInsert tree path 'x') Empty insertPaths

-- Problem 56

mirror Empty          Empty            = True
mirror (Branch _ l r) (Branch _ l' r') = mirror l r' && mirror r l'
mirror _              _                = False

symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

-- Problem 57

add :: Ord a => a -> Tree a -> Tree a
add x Empty            = leaf x
add x t@(Branch y l r) =
  case compare x y of
    EQ -> t
    GT -> Branch y l (add x r)
    LT -> Branch y (add x l) r

construct :: Ord a => [a] -> Tree a
construct = foldl (flip add) Empty

testSymmetric :: Ord a => [a] -> Bool
testSymmetric = symmetric . construct
