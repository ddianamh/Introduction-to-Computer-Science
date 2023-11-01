module Main (main) where

import Test.HUnit

--AND
--could have used && directly, without defining land, but I wanted to practice defining functions
land :: Bool -> Bool -> Bool
land True True = True
land _ _ = False

--OR
--could have used ||
lor :: Bool -> Bool -> Bool
lor False False = False
lor _ _ = True

--NOT
--could have used not
lnot :: Bool -> Bool
lnot False = True
lnot True = False

--IMPLIES
-- x `limplies` y = ( not x ) || y
limplies :: Bool -> Bool -> Bool
limplies True False = False
limplies _ _ = True

--The function brandy implements the rules directly

brandy :: Bool -> Bool -> Bool -> Bool 
brandy l m n = (l `limplies` m ) `land` ( `lnot` (m `land` n )) `land` (l `limplies` m )

--The function brandy' implements the simplified formula
brandy' :: Bool -> Bool -> Bool -> Bool 
brandy' l m n = l `land` m `land` ( `lnot` n )

--The function truthTable takes a function as an argument and returns
--a list where each element is a 4-tuple representing three input
--values passed to the function followed by the function's result

truthTable :: (Bool -> Bool -> Bool -> Bool) -> [(Bool -> Bool -> Bool -> Bool)]
truthTable brandy l m n = [(l, m, n, x) | x <- brandy l m n ]
truthTable brandy' l m n = [(l, m, n, x) | x <- brandy' l m n ]

brandyTests = TestList [ truthTable brandy False False False ~?= truthTable brandy' False False False
                        ,truthTable brandy False False True ~?= truthTable brandy' False False True 
                        ,truthTable brandy False True False ~?= truthTable brandy' False True False 
                        ,truthTable brandy False True True ~?= truthTable brandy' False True True 
                        ,truthTable brandy True False False ~?= truthTable brandy' True False False 
                        ,truthTable brandy True False True ~?= truthTable brandy' True False True 
                        ,truthTable brandy True True False ~?= truthTable brandy' True True False 
                        ,truthTable brandy True True True ~?= truthTable brandy' True True True ]

main = runTestTT brandyTests