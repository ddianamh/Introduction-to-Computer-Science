module Main (main) where
import Test.HUnit

--function should have S_0 = 1
--S_n = (S_{n-1}) + the sum of the elements of the list
--containing elements of type S_k * S_{n-k-1}, where
--k goes from 0 to n-1

s :: Integral a => a -> a
s 0 = 1
s n = s (n-1) + sum [  s k * s (n-k-1) | k <- [0..(n-1)] ]

sTests = 
    TestList [ s 0 ~?= 1
             , s 1 ~?= 2
             , s 3 ~?= 22
             , s 7 ~?= 8558
             , s 10 ~?= 1037718
             , s 20 ~?= 17518619320890
             ]
main = runTestTT $ TestList [sTests]