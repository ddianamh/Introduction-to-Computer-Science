module EditDistanceTest (main) where
import EditDistance
import Test.HUnit

EditDistTests = TestList
                [
                    ed [] [] ~?= 0,
                    ed "" "" ~?= 0,
                    ed "abc" "" ~?= 3,
                    ed "" "abc" ~?= 3,
                    ed [1,2,3,4] [] ~?= 4,
                    ed [1,2] [1,2] ~?= 0,
                    ed "same" "same" ~?= 0,
                    ed [0,1,2,3] [1,2,3] ~?= 1,
                    ed [0,0,0,0] [1,2,3] ~?= 4,
                    ed "diana" "viana" ~?= 1,
                    ed "word" "yesss!" ~?= 6
                ]

main = runTestTT EditDistTEsts