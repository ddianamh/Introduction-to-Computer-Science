import Prelude hiding (reverse, product, gcd, sum)
--Euclidian GCD Algorithm
gcd :: Integral a => a -> a -> a
gcd a 0 = a
gcd a b = gcd b ( a `mod` b)

--Sum
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

--Product
product ::  Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs 

--Reversing a list
reverse :: [a] -> [a]
reverse [] = []
reverse xs = last xs : reverse (init xs)


--sumTests = TestList [ sum [] ~?= 0
--                    , sum [42] ~?= 42
--                    , sum [1..10] ~?= 55
--                    , sum [-10..0] ~?= (-55)
--                    , sum [-10..10] ~?= 0
--                    ] 

--main = runTestTT sumTests
