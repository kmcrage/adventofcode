{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore: Use second -}
-- cabal run --enable-profiling day16 --  +RTS -p
import           Control.Arrow

import qualified Data.List     as L
import qualified Data.Map      as M
import           Debug.Trace

import qualified IntCode       as IC

type Coord = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "data/test.dat"
  contents <- readFile "data/2019-12-19.dat"
  let intcodes = IC.readIntcode contents
      part1 = countInSquare intcodes 50
      (part2X, part2Y) = diagonal intcodes 100 (0, 100)
      result = 10000 * part2X + part2Y
  putStrLn $ "Part One: " ++ show part1
  putStrLn $ "Part Two: " ++ show result ++ " " ++ show (part2X, part2Y)

diagonal :: [Int] -> Int -> Coord -> Coord
diagonal intcodes n (x, y)
  | not bl = diagonal intcodes n (x + 1, y)
  | tr = (x, y - n + 1)
  | otherwise = diagonal intcodes n (x, y + 1)
  where
    bl = beam intcodes (x, y) == 1
    tr = beam intcodes (x + n - 1, y - n + 1) == 1

countInSquare :: [Int] -> Int -> Int
countInSquare intcodes n = length hits
  where
    range = [0 .. (n - 1)]
    hits =
      map (\c -> 1 == beam intcodes c) >>> filter (id) $
      [(x, y) | x <- range, y <- range]

beam :: [Int] -> Coord -> Int
beam intcodes (i, j) = head $ IC.output m
  where
    m = IC.runPrg intcodes [i, j]
