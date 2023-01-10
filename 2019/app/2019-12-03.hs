import           Data.List
import           Data.List.Split
import qualified Data.Map        as Map
import qualified Data.Set        as Set

type Coord = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "data/2019-12-03.dat"
    -- contents <- readFile "data/test.dat"
  let wireLists = map wireCoords $ lines contents
      coordSets = map Set.fromList wireLists
      intersections = head coordSets `Set.intersection` last coordSets
      mdists = Set.map manhattan intersections
      mresult = minimum mdists
      wdists = wiredist intersections wireLists
      wresult = minimum wdists
  putStrLn $ "Min dist: " ++ show mresult
  putStrLn $ "Wire dist: " ++ show wresult

wiredist :: Set.Set Coord -> [[Coord]] -> Set.Set Int
wiredist is cs = Set.map (wiredistance dmaps) is
  where
    dmaps = map distances cs

wiredistance :: [Map.Map Coord Int] -> Coord -> Int
wiredistance dmaps c = sum $ map (Map.! c) dmaps

distances :: [Coord] -> Map.Map Coord Int
distances ds = Map.fromListWith min $ zip ds [1 ..]

wireCoords :: String -> [Coord]
wireCoords ds = do
  let ds' = splitOn "," ds
      pos = (0, 0)
  tail $ wireCoordsExpand [pos] ds'

wireCoordsExpand :: [Coord] -> [String] -> [Coord]
wireCoordsExpand poss [] = poss
wireCoordsExpand poss ds
  | dir == 'U' = wireCoordsExpand (poss ++ path pos (1, 0) [1 .. dist]) ds'
  | dir == 'D' = wireCoordsExpand (poss ++ path pos (-1, 0) [1 .. dist]) ds'
  | dir == 'L' = wireCoordsExpand (poss ++ path pos (0, -1) [1 .. dist]) ds'
  | dir == 'R' = wireCoordsExpand (poss ++ path pos (0, 1) [1 .. dist]) ds'
  | otherwise = poss
  where
    motion = head ds
    ds' = tail ds
    dir = head motion
    dist = read (tail motion) :: Int
    pos = last poss

path :: Coord -> Coord -> [Int] -> [Coord]
path (px, py) (dx, dy) = map (\n -> (px + n * dx, py + n * dy))

manhattan :: Coord -> Int
manhattan (x, y) = abs x + abs y
