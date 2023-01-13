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
      maxvispt = maximum $ L.map ((`visible` ptsSet) &&& id) pts
      maxvis = fst maxvispt
      maxpt = snd maxvispt
      pts' = filter (/= maxpt) pts
      sorted = sortByDeletion maxpt pts'
      pt200 = sorted !! (200 - 1)
      result = 100 * fst pt200 + snd pt200
  putStrLn $ "Max vis asteroids: " ++ show maxvis ++ " at " ++ show maxpt
  putStrLn $ "Part Two: " ++ show result

sortByDeletion :: Coord -> [Coord] -> [Coord]
sortByDeletion origin =
  map (diff origin &&& id) >>>
  L.sortOn (manhattan . fst) >>>
  E.groupSortOn (toAngle . fst) >>> map (map snd) >>> laser

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

calcLayer :: (Coord, [(Coord, Coord)]) -> [(Int, Coord, Coord)] -- layer, diff, coord
calcLayer (direction, ocs) = ocsLayered
  where
    ocsSorted = L.sortOn (\((a, b), _) -> abs a + abs b) ocs
    ocsLayered = zipWith (curry (\(a, (b, c)) -> (a, b, c))) [0 ..] ocsSorted

visible :: Coord -> S.Set Coord -> Int
visible coord coords = cnt
  where
    others = S.filter (/= coord) coords
    cnt = length $ S.map (direction coord) others

diff :: Coord -> Coord -> Coord
diff (a, b) (x, y) = (x - a, y - b)

direction :: Coord -> Coord -> Coord
direction a b = (div d n, div e n)
  where
    (d, e) = diff a b
    n = gcd (abs d) (abs e)

parse :: String -> [Coord]
parse contents = pts'
  where
    numlines = zip [0 ..] (lines contents)
    ptsRaw =
      concatMap
        (\(j, line) -> zipWith (\i char -> (i, j, char)) [0 ..] line)
        numlines
    pts = filter (\(i, j, c) -> c == '#') ptsRaw
    pts' = map (\(i, j, c) -> (i, j)) pts
