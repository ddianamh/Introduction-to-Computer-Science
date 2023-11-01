module Main (main) where

import Test.HUnit

--Convert a non-negative integer number into a String 
--providing a binary representation of the number.

--doing it recurssively, by concatentation
dtob :: Int -> String
dtob 0 = "0"
dtob 1 = "1"
dtob n = ( dtob (n `div` 2) ) ++ ( dtob (n `mod` 2) )

--Convert a String representing a non-negative integer number
--as a binary number into an integer number.

--doing it recurssively
--if the head of the string is the char 1, then add the corresponding
--power of 2 to btod of the tail
btod :: String -> Int
btod [] = 0
btod (x:xs) 
    | x == '1' = 2 ^ ( length (xs) ) + ( btod xs )
    | otherwise = btod xs

dtobTests = TestList[ dtob 0 ~?= "0"
                    , dtob 1 ~?= "1"
                    , dtob 2 ~?= "10"
                    , dtob 127 ~?= "1111111"
                    , dtob 12345 ~?= "11000000111001"
                    ]

btodTests = TestList[ btod "0" ~?= 0
                    , btod "1" ~?= 1
                    , btod "10" ~?= 2
                    , btod "1111111" ~?= 127
                    , btod "11000000111001" ~?= 12345
                    ]


main = runTestTT $ TestList [ dtobTests, btodTests ]