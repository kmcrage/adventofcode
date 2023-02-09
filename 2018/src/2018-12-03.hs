{-# LANGUAGE TypeApplications #-}

import           Control.Arrow
import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Debug.Trace

type Claims = M.Map Int (Coord, Coord)

type Overlaps = M.Map Coord Int

type Coord = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "data/2018-12-03.dat"
  -- contents <- readFile "data/test.dat"
  let claims = parse contents
      ols = overlaps claims
      claim = noOverlap ols claims
  putStrLn $ "Part 1, overlaps: " ++ show (length ols)
  putStrLn $ "Part 2, no overlap claim: " ++ show claim

noOverlap :: Overlaps -> Claims -> Int
noOverlap olapMap claims
  | olaps == 0 = num
  | otherwise = noOverlap olapMap claims'
  where
    ((num, ((i, j), (si, sj))), claims') = M.deleteFindMin claims
    olaps =
      filter (`M.member` olapMap) >>> length $
      [(i + di, j + dj) | di <- [0 .. (si - 1)], dj <- [0 .. (sj - 1)]]

overlaps :: Claims -> Overlaps
overlaps = M.foldl' applyClaim M.empty >>> M.filter (>= 2)

applyClaim :: M.Map Coord Int -> (Coord, Coord) -> M.Map Coord Int
applyClaim cloth ((i, j), (si, sj)) =
  L.foldl'
    (\m c -> M.insertWith (+) c 1 m)
    cloth
    [(i + di, j + dj) | di <- [0 .. (si - 1)], dj <- [0 .. (sj - 1)]]

parse :: String -> Claims
parse = lines >>> map toClaim >>> M.fromList

toClaim :: String -> (Int, (Coord, Coord))
toClaim line = (num, (pos, size))
  where
    num = L.splitOn " " >>> head >>> filter (/= '#') >>> read @Int $ line
    pos =
      L.splitOn " " >>>
      drop 2 >>> head >>> filter (/= ':') >>> L.splitOn "," >>> toCoord $
      line
    size =
      L.splitOn " " >>>
      drop 3 >>> head >>> filter (/= ':') >>> L.splitOn "x" >>> toCoord $
      line
    toCoord = read . head &&& read . last
