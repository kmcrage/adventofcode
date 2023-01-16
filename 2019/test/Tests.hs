{-# LANGUAGE BlockArguments #-}

module Main
  ( main
  ) where

import           Control.Arrow
import           Data.List.Split
import           IntCode               as IC
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           IntCode               (Machine (input))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [quickCheckTests, hunitTests]

quickCheckTests :: TestTree
quickCheckTests =
  testGroup
    "QuickCheck Tests"
    [QC.testProperty "Compare result with 8" prop_numericComparison]

hunitTests :: TestTree
hunitTests =
  testGroup "HUnit Tests" [test1, test2, test3, test4, test5, test6, test7]

test1 =
  testCase "day02" $
  assertEqual
    "day02"
    (memoryTest [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] [])
    [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]

test2 =
  testCase "day05.1 less than 8" $
  assertEqual
    "day05.1 less than 8"
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
    [999]

test3 =
  testCase "day05.2 equal 8" $
  assertEqual
    "day05.2 equal 8"
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
    [1000]

test4 =
  testCase "day05.3 greater than 8" $
  assertEqual
    "day05.2 equal 8"
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
    [1001]

test5 =
  testCase "day09.1 duplicate" $
  assertEqual
    "day05.2 equal 8"
    (outputTest
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
       ]
       [1])
    [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]

test6 =
  testCase "day09.2 extract" $
  assertEqual
    "day09.2 extract"
    (outputTest [104, 1125899906842624, 99] [])
    [1125899906842624]

test7 =
  testCase "day09.3 multiply" $
  assertEqual
    "day09.3 multiply"
    (outputTest [1102, 34915192, 34915192, 7, 4, 7, 99, 0] [])
    [34915192 * 34915192]

memoryTest :: [Int] -> [Int] -> [Int]
memoryTest intcodes input = IC.memory $ IC.runPrg intcodes input

outputTest :: [Int] -> [Int] -> [Int]
outputTest intcodes input = IC.output $ IC.runPrg intcodes input

cmp8 :: Int -> Int
cmp8 n
  | n < 8 = 999
  | n == 8 = 1000
  | otherwise = 1001

prop_numericComparison :: Int -> Bool
prop_numericComparison input = head result == cmp8 input
  where
    result = IC.output $ IC.runPrg intcodes [input]
    intcodes =
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
