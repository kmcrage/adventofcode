import           Control.Arrow
import qualified Data.List       as L
import           Data.List.Extra as E
import qualified Data.Map        as M
import qualified Data.Set        as S

type Coord = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "data/2019-12-10.dat"
  -- contents <- readFile "data/test.dat"
  let pts = parse contents
      ptsSet = S.fromList pts
      (maxvis, maxpt) = maximum $ L.map ((`visible` ptsSet) &&& id) pts
      pts' = filter (/= maxpt) pts
      sorted = sortByDeletion maxpt pts'
      (rx, ry) = sorted !! (200 - 1)
      result = 100 * rx + ry
  putStrLn $ "Max vis asteroids: " ++ show maxvis ++ " at " ++ show maxpt
  putStrLn $ "Part Two: " ++ show result

sortByDeletion :: Coord -> [Coord] -> [Coord]
sortByDeletion origin =
  L.sortOn (manhattan . diff origin) >>>
  E.groupSortOn (toAngle . diff origin) >>> laser

laser :: [[Coord]] -> [Coord]
laser [] = []
laser list = map head list ++ laser rest
  where
    rest = map tail >>> filter (not . null) $ list

manhattan :: Coord -> Int
manhattan (a, b) = abs a + abs b

toAngle :: Coord -> Float
toAngle (a, b)
  | a == 0 && b < 0 = 0
  | otherwise = pi - atan2 (fromIntegral a) (fromIntegral b)

visible :: Coord -> S.Set Coord -> Int
visible coord = length . directions
  where
    directions = S.filter (/= coord) >>> S.map (direction coord)

diff :: Coord -> Coord -> Coord
diff (a, b) (x, y) = (x - a, y - b)

direction :: Coord -> Coord -> Coord
direction a b = (div d n, div e n)
  where
    (d, e) = diff a b
    n = gcd (abs d) (abs e)

parse :: String -> [Coord]
parse =
  lines >>>
  zip [0 ..] >>>
  concatMap (\(j, line) -> zipWith (\i char -> (i, j, char)) [0 ..] line) >>>
  filter (\(i, j, c) -> c == '#') >>> map (\(i, j, c) -> (i, j))
