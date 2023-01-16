module Main
  ( main
  ) where

import           Control.Arrow
import           Data.List.Split
import           IntCode
import qualified System.Exit     as Exit
import           Test.HUnit
import           Test.QuickCheck (quickCheck)

tests =
  TestList
    [ TestLabel "day02" test1
    , TestLabel "day05.1 less than 8" test2
    , TestLabel "day05.2 equal to 8" test3
    , TestLabel "day05.3 greater than 8" test4
    , TestLabel "day09.1 list duplication" test5
    , TestLabel "day09.2 int duplication" test6
    , TestLabel "day09.3 multiplication" test7
    ]

test1 =
  TestCase
    (assertEqual
       "day02"
       (memoryTest [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [])
       [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50])

test2 =
  TestCase
    (assertEqual
       "day05.1"
       (outputTest
          [ 3
          , 21
          , 1008
          , 21
          , 8
          , 20
          , 1005
          , 20
          , 22
          , 107
          , 8
          , 21
          , 20
          , 1006
          , 20
          , 31
          , 1106
          , 0
          , 36
          , 98
          , 0
          , 0
          , 1002
          , 21
          , 125
          , 20
          , 4
          , 20
          , 1105
          , 1
          , 46
          , 104
          , 999
          , 1105
          , 1
          , 46
          , 1101
          , 1000
          , 1
          , 20
          , 4
          , 20
          , 1105
          , 1
          , 46
          , 98
          , 99
          ]
          [7])
       [999])

test3 =
  TestCase
    (assertEqual
       "day05.2"
       (outputTest
          [ 3
          , 21
          , 1008
          , 21
          , 8
          , 20
          , 1005
          , 20
          , 22
          , 107
          , 8
          , 21
          , 20
          , 1006
          , 20
          , 31
          , 1106
          , 0
          , 36
          , 98
          , 0
          , 0
          , 1002
          , 21
          , 125
          , 20
          , 4
          , 20
          , 1105
          , 1
          , 46
          , 104
          , 999
          , 1105
          , 1
          , 46
          , 1101
          , 1000
          , 1
          , 20
          , 4
          , 20
          , 1105
          , 1
          , 46
          , 98
          , 99
          ]
          [8])
       [1000])

test4 =
  TestCase
    (assertEqual
       "day05.3"
       (outputTest
          [ 3
          , 21
          , 1008
          , 21
          , 8
          , 20
          , 1005
          , 20
          , 22
          , 107
          , 8
          , 21
          , 20
          , 1006
          , 20
          , 31
          , 1106
          , 0
          , 36
          , 98
          , 0
          , 0
          , 1002
          , 21
          , 125
          , 20
          , 4
          , 20
          , 1105
          , 1
          , 46
          , 104
          , 999
          , 1105
          , 1
          , 46
          , 1101
          , 1000
          , 1
          , 20
          , 4
          , 20
          , 1105
          , 1
          , 46
          , 98
          , 99
          ]
          [9])
       [1001])

test5 =
  TestCase
    (assertEqual
       "day09.1"
       (outputTest
          ([ 109
           , 1
           , 204
           , -1
           , 1001
           , 100
           , 1
           , 100
           , 1008
           , 100
           , 16
           , 101
           , 1006
           , 101
           , 0
           , 99
           ] ++
           repeat 0)
          [1])
       [ 109
       , 1
       , 204
       , -1
       , 1001
       , 100
       , 1
       , 100
       , 1008
       , 100
       , 16
       , 101
       , 1006
       , 101
       , 0
       , 99
       ])

test6 =
  TestCase
    (assertEqual
       "day09.2"
       (outputTest [104, 1125899906842624, 99] [])
       [1125899906842624])

test7 =
  TestCase
    (assertEqual
       "day09.3"
       (outputTest [1102, 34915192, 34915192, 7, 4, 7, 99, 0] [])
       [34915192 * 34915192])

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0
    then Exit.exitFailure
    else Exit.exitSuccess

memoryTest :: [Int] -> [Int] -> [Int]
memoryTest intcodes input = memory $ runPrg intcodes input

outputTest :: [Int] -> [Int] -> [Int]
outputTest intcodes input = output $ runPrg intcodes input
