module Main (main) where

import Test.HUnit
import Distribution.Simple.Utils (xargs)

powLin :: Integer -> Integer -> Integer
powLin x n
    | n == 0 = 1
    | otherwise = x * powLin x (n-1)

powLog :: Integer -> Integer -> Integer
powLog x n 
    | n == 0 = 1
    | even n = ( powLog x (div n 2) ) * ( powLog x (div n 2) )
    | otherwise = x * ( powLog x (n - 1) )

go :: Integer -> Integer -> Integer -> Integer
go n a1 a2
    | n == 0 = 1
    | n == 1 = a1 * a2
    | even n = go (div n 2) a1 (a2 * a2)
    | otherwise = go (n-1) (a1 * a2) a2


powTail :: Integer -> Integer -> Integer
powTail x n = go n 1 x

powLinTests = TestList  [ map (powLin 0) [0,1,2,3,10] ~?= [1,0,0,0,0] 
                        , map (powLin 2) [0,1,2,3,10] ~?= [1,2,4,8,1024]
                        , map (powLin 5) [0,1,2,3,10] ~?= [1,5,25,125,9765625]
                        ]

powLogTests = TestList  [ map (powLog 0) [0,1,2,3,10] ~?= [1,0,0,0,0] 
                        , map (powLog 2) [0,1,2,3,10] ~?= [1,2,4,8,1024]
                        , map (powLog 5) [0,1,2,3,10] ~?= [1,5,25,125,9765625]
                        ]

powTailTests = TestList [ map (powTail 0) [0,1,2,3,10] ~?= [1,0,0,0,0] 
                        , map (powTail 2) [0,1,2,3,10] ~?= [1,2,4,8,1024]
                        , map (powTail 5) [0,1,2,3,10] ~?= [1,5,25,125,9765625]
                        ]

main = runTestTT $ TestList [powLinTests, powLogTests, powTailTests]