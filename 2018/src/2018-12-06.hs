{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow
import qualified Data.Char       as C
import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Set        as S
import           Debug.Trace

type Coord = (Int, Int)

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
    (a,b) = L.foldl' (\(i,j) (k,l) -> (i+k,j+l)) (0,0) pts
    l = length pts

maxFiniteArea :: [Coord] -> Int
maxFiniteArea points = area points 0 points

-- full list, max area, queue
area :: [Coord] -> Int -> [Coord] -> Int
area _ mx [] = mx
area points mx (p:queue) = area points mx' queue
  where
    a = paint mxDist p points S.empty [p]
    mx' = max a mx
    mxDist = map (manDist p) >>> maximum $ points

-- maxDist, current point, all points, area coords, queue -> area
paint :: Int -> Coord -> [Coord] -> S.Set Coord -> [Coord] -> Int
paint _ _ _ visited [] = length visited
paint mx p points visited (q:queue)
  | S.member q visited = paint mx p points visited queue
  | qp < qmin = paint mx p points visited' (queue ++ nhbrs)
  | qp > mx = 0
  | otherwise = paint mx p points visited queue
  where
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
