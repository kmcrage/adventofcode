{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow
import qualified Data.Char       as C
import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Set        as S
import           Debug.Trace

type Coord = (Int, Int)

type BBox = (Int, Int, Int, Int)

main :: IO ()
main = do
  contents <- readFile "data/2018-12-06.dat"
  -- contents <- readFile "data/test.dat"
  let points = parse contents
      part1 = maxFiniteArea points
      part2 = nearbyArea 10000 points
  putStrLn $ "Part 1, min: " ++ show part1
  putStrLn $ "Part 2, sum: " ++ show part2

nearbyArea :: Int -> [Coord] -> Int
nearbyArea mx pts = paintMax mx pts S.empty [c]
  where
    c = avePt pts

paintMax :: Int -> [Coord] -> S.Set Coord -> [Coord] -> Int
paintMax _ _ visited [] = length visited
paintMax mx points visited (q:queue)
  | S.member q visited = paintMax mx points visited queue
  | qpsum < mx = paintMax mx points visited' (queue ++ nhbrs)
  | otherwise = paintMax mx points visited queue
  where
    qpsum = map (manDist q) >>> sum $ points
    visited' = S.insert q visited
    (i, j) = q
    nhbrs =
      map (\(di, dj) -> (i + di, j + dj)) >>> filter (`S.notMember` visited) $
      [(0, 1), (0, -1), (1, 0), (-1, 0)]

avePt :: [Coord] -> Coord
avePt pts = (div a l, div b l)
  where
    (a, b) = L.foldl' (\(i, j) (k, l) -> (i + k, j + l)) (0, 0) pts
    l = length pts

maxFiniteArea :: [Coord] -> Int
maxFiniteArea points = area (bbXmin, bbYmin, bbXmax, bbYmax) points 0 points
  where
    bbXmin = map fst >>> minimum $ points
    bbYmin = map snd >>> minimum $ points
    bbXmax = map fst >>> maximum $ points
    bbYmax = map snd >>> maximum $ points

-- full list, max area, queue
area :: BBox -> [Coord] -> Int -> [Coord] -> Int
area _ _ mx [] = mx
area bbox points mx (p:queue) = area bbox points mx' queue
  where
    mx' = max mx $ paint bbox p points S.empty [p]

-- maxDist, current point, all points, area coords, queue -> area
paint :: BBox -> Coord -> [Coord] -> S.Set Coord -> [Coord] -> Int
paint _ _ _ visited [] = length visited
paint bbox p points visited (q:queue)
  | S.member q visited = paint bbox p points visited queue
  | qp < qmin = paint bbox p points visited' (queue ++ nhbrs)
  | i == imn || i == imx || j == jmn || j == jmx = 0
  | otherwise = paint bbox p points visited queue
  where
    (imn, jmn, imx, jmx) = bbox
    qp = manDist q p
    qmin = filter (/= p) >>> map (manDist q) >>> minimum $ points
    visited' = S.insert q visited
    (i, j) = q
    nhbrs =
      map (\(di, dj) -> (i + di, j + dj)) >>> filter (`S.notMember` visited) $
      [(0, 1), (0, -1), (1, 0), (-1, 0)]

manDist :: Coord -> Coord -> Int
manDist (a, b) (c, d) = abs (a - c) + abs (b - d)

parse :: String -> [Coord]
parse =
  lines >>> map (L.splitOn ", ") >>> map (\l -> (read (head l), read (last l)))
