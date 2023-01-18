import           Control.Arrow

data MoonGeneric a =
  Moon
    { position :: a
    , velocity :: a
    }
  deriving (Show, Eq)

type Moon = MoonGeneric [Int]
type MoonCoord = MoonGeneric Int

main :: IO ()
main = do
  contents <- readFile "data/2019-12-12.dat"
  -- contents <- readFile "data/test.dat"
  let start = parse contents
      systems = iterate simSystem start
      energy = calcEnergy (systems !! 1000)
      period = calcPeriod systems
  putStrLn $ "Part 1 energy: " ++ show energy
  putStrLn $ "Part 2 period: " ++ show period

calcPeriod :: [[Moon]] -> Int
calcPeriod systems = foldr (lcm . calcCoordPeriod systems) 1 [0 .. 2]

calcCoordPeriod :: [[Moon]] -> Int -> Int
calcCoordPeriod systems i =
  zip [0 ..] >>>
  tail >>>
  dropWhile (\(n, sys) -> getMoonCoords i sys /= startCoords) >>> head >>> fst $
  systems
  where
    start = head systems
    startCoords = getMoonCoords i start

getMoonCoords :: Int -> [Moon] -> [MoonCoord]
getMoonCoords i = map (getMoonCoord i)

getMoonCoord :: Int -> Moon -> MoonCoord
getMoonCoord i m = Moon {velocity = velocity m !! i, position = position m !! i}

simSystem :: [Moon] -> [Moon]
simSystem ms = ms''
  where
    ms' = map (updateVelocity ms) ms
    ms'' = map updatePosition ms'

updateVelocity :: [Moon] -> Moon -> Moon
updateVelocity system moon = moon {velocity = v'}
  where
    p = position moon
    v = velocity moon
    v' = foldr (updateVelocity1 p . position) v system

updateVelocity1 :: [Int] -> [Int] -> [Int] -> [Int]
updateVelocity1 p1 p2 vel =
  zipWith (+) vel $ zipWith (\a b -> signum (b - a)) p1 p2

updatePosition :: Moon -> Moon
updatePosition m = m {position = p'}
  where
    p' = zipWith (+) (position m) (velocity m)

initMoon :: [Int] -> Moon
initMoon p = Moon {position = p, velocity = replicate 3 0}

calcEnergy :: [Moon] -> Int
calcEnergy =
  foldr (\m -> (+) (sumabs (position m) * sumabs (velocity m))) 0
  where sumabs = sum . map abs 

parse :: [Char] -> [Moon]
parse =
  filter (`notElem` "<>xyz= ") >>>
  lines >>> map (\l -> "[" ++ l ++ "]") >>> map (initMoon . read)
