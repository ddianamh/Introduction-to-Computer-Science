module Main (main) where

import Test.HUnit

-- The function brandy implements the rules directly.
-- x `limplies` y = ( not x ) || y
brandy :: Bool -> Bool -> Bool -> Bool
brandy l m n = (not l || m) &&  (not (m && n)) && (l || n) && (not n || l)

-- The function brandy' implements the simplified formula.
brandy' :: Bool -> Bool -> Bool -> Bool
brandy' l m n = l && m && (not n)

-- The function truthTable takes a function as an argument and returns
-- a list where each element is a 4-tuple representing three input
-- value passed to the function followed by the function's result.
truthTable :: (Bool -> Bool -> Bool -> Bool) -> [(Bool, Bool, Bool, Bool)]
truthTable f = [(x,y,z,a) | x<-[False, True], y<-[False, True], z<-[False, True], a<-[True, False], a == f x y z] 


-- Test whether the two truth tables returned are the same (which is
-- not a very sharp test but I do not want to reveal too many details).
-- You may want to add your own test cases...
brandyTests = TestList [ truthTable brandy ~?= truthTable brandy'
                        ,brandy False False True ~?= brandy' False False True
                        ,brandy False False True ~?= brandy' False False True 
                        ,brandy False True False ~?= brandy' False True False 
                        ,brandy False True True ~?= brandy' False True True 
                        ,brandy True False False ~?= brandy' True False False 
                        ,brandy True False True ~?= brandy' True False True 
                        ,brandy True True False ~?= brandy' True True False 
                        ,brandy True True True ~?= brandy' True True True]

main = runTestTT brandyTests