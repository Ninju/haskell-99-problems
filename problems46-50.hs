module Main where
import Prelude hiding (and, or)

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
