module Main (main) where

import Test.HUnit


data Exp = C Int -- a constant integer
         | V String -- a variable with a name
         | S Exp Exp -- a sum of two expressions
         | P Exp Exp -- a product of two expressions
         deriving (Show, Eq)

simplify :: Exp -> Exp
simplify C x = C x
simplify V x = V x
simplify (S (C x) (C y) ) = C (x+y)
simplify (S (V x) (C 0) ) = V x
simplify (S (C 0) (V x) ) = V x
simplify (S (V x) (C y) ) = C y + V x
simplify (S (C y) (V x) ) = C y + V x
simplify (S (V x) (V y) ) = V x + V y
simplify (P (C x) (C y) ) = C (x * y)
simplify (P (V x) (C 1) ) = V x
simplify (P (C 1) (V x) ) = V x
simplify (P (V x) (C 0) ) = C 0
simplify (P (C 0) (V x) ) = C 0
simplify (S (V x) (C y) ) = C y * V x
simplify (S (C y) (V x) ) = C y * V x
simplify (S (V x) (V y) ) = V x * V y

tI0 = TestList
    [
    simplify (C3)~?=C3 --3=3
    , simplify (V "y") ~?= V "y" -- y=y
    ]

tS1 = TestList
    [
    simplify (S(C3)(C5))~?=C8 -- 3+5=8
    ]

tS2 = TestList
    [
    simplify (S (C 0) (V "y")) ~?= V "y"  -- 0+y=y
    , simplify (S (V "y") (C 0)) ~?= V "y" -- y+0=y
    ]

tS3 = TestList
    [ simplify (S (S (C 3) (V "y")) (C 5)) ~?= S (C 8) (V "y")
    , simplify (S (S (V "y") (C 3)) (C 5)) ~?= S (C 8) (V "y")   
    , simplify (S (C 3) (S (C 5) (V "y"))) ~?= S (C 8) (V"y") 
    , simplify (S (C 3) (S (V "y")(C 5))) ~?= S (C 8) (V "y") ]

tS4 = TestList
[ simplify (S (S (C 3) (C 5))(C 8)) ~?= C 16
, simplify (S (C 3)(S (C 5) (C 8))) ~?= C 16
, simplify (S (C 5) (V "y")) ~?= S (C 5) (V "y")
, simplify (S (V "y") (C 5)) ~?= S (V "y") (C 5)
, simplify (S (V "x") (V "y")) ~?= S (V "x") (V "y") ]

tP1 = TestList
[ simplify (P (C 3) (C 5))~?= C 15
]

tP2 = TestList
[
simplify (P (C 1) (V "y")) ~?= V "y"
, simplify (P (V "y") (C 1)) ~?= V "y"
]

tP3 = TestList
[
simplify (P (C 0) (V "y")) ~?= C 0
, simplify (P (V "y") (C 0)) ~?= C 0
]

tP4 = TestList
[
simplify (P (P (C 3) (V "y")) (C 2)) ~?= P (C 6) (V "y")
, simplify (P (P (V "y") (C 3)) (C 2)) ~?= P (C 6) (V "y")
, simplify (P (C 3) (P (C 2) (V "y"))) ~?= P (C 6) (V "y")
, simplify (P (C 3) (P (V "y") (C 2))) ~?= P (C 6) (V "y")
]

tP5 = TestList
[
simplify (P(P (C 3) (C 5)) (C 8)) ~?= C 120
, simplify (P (C 3) (P (C 5) (C 8))) ~?= C 120
, simplify (P (C 5) (V "y")) ~?= P (C 5) (V "y")
, simplify (P (V "y") (C 5)) ~?= P (V "y") (C 5)
, simplify (P (V "x") (V "y")) ~?= P (V "x") (V "y")
]

tM0 = TestList[
    simplify (P (P (C 2) (V "y")) (S (C 3) (P (C 2) (C 2)))) ~?= P (C 14) (V "y")
,   simplify (S (V "x") (S (C 1) (C (-1)))) ~?= V "x"
,   simplify (P (S (C 1) (C (-1))) (V "x")) ~?= C 0
,   simplify (P (S (C 2) (C (-1))) (V "x")) ~?= V "x"
,   simplify (P (P (C 2) (C 2)) (S (C 3) (C 4))) ~?= C 28
]

main = runTestTT $ TestList [tI0, tS1, tS2, tS3, tP1, tP2, tP3, tP4, tP5, tM0]