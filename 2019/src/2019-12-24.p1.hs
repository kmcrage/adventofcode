{- HLINT ignore: Use second -}
import           Control.Arrow
import qualified Data.List       as L
import           Data.List.Extra as E
import qualified Data.Map        as M
import qualified Data.Set        as S

import           Debug.Trace

type Coord = (Int, Int)

type Bugs = S.Set Coord

main :: IO ()
main = do
  contents <- readFile "data/2019-12-24.dat"
  -- contents <- readFile "data/test.dat"
  let bugs = parse contents
      part1 = findRepeat bugs S.empty
      result1 = S.toList >>> map (\(i, j) -> 2 ^ (5 * i + j)) >>> sum $ part1
  putStrLn $ "Part One: " ++ show result1

findRepeat :: Bugs -> S.Set Bugs -> Bugs
findRepeat bugs previous
  | S.member bugs' previous = bugs'
  | otherwise = findRepeat bugs' previous'
  where
    previous' = S.insert bugs' previous
    bugs' =
      filter (isBug bugs) >>> S.fromList $
      [(i, j) | i <- [0 .. 4], j <- [0 .. 4]]

isBug :: Bugs -> Coord -> Bool
isBug bugs (i, j)
  | S.member (i, j) bugs && nghrs == 1 = True
  | S.notMember (i, j) bugs && nghrs == 1 = True
  | S.notMember (i, j) bugs && nghrs == 2 = True
  | otherwise = False
  where
    nghrs =
      map (\(di, dj) -> (i + di, j + dj)) >>>
      filter (`S.member` bugs) >>> length $
      [(0, 1), (1, 0), (0, -1), (-1, 0)]

view :: Bugs -> String
view bugs = map toChr [(i, j) | i <- [0 .. 4], j <- [0 .. 5]]
  where
    toChr (i, j)
      | S.member (i, j) bugs = '#'
      | j == 5 = '\n'
      | otherwise = '.'

parse :: String -> Bugs
parse =
  lines >>>
  zip [0 ..] >>>
  concatMap coordChar >>>
  filter (\(i, j, c) -> c == '#') >>>
  map (\(i, j, c) -> (j, i)) >>> S.fromList

coordChar :: (Int, String) -> [(Int, Int, Char)]
coordChar (j, line) = zipWith (\i char -> (i, j, char)) [0 ..] line
