{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow        ((&&&), (>>>))
import qualified Data.Char            as C
import qualified Data.List            as L
import qualified Data.Map             as M
import qualified Data.Maybe           as J
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Set             as S
import           Debug.Trace          (trace)

type Cave = S.Set Coord

type State = (Cave, [Unit])

type Coord = (Int, Int)

type Visited = M.Map (Coord, Coord) Int

type Queue = Q.MinPQueue Int Unit

data Species
  = Elf
  | Goblin
  deriving (Enum, Eq, Show, Ord)

data Unit =
  Unit
    { position :: Coord
    , path     :: [Coord]
    , species  :: Species
    , health   :: Int
    , attack   :: Int
    , victor   :: Bool
    }
  deriving (Eq, Ord, Show)

defaultElf :: Unit
defaultElf = Unit (0, 0) [] Elf 200 3 False

defaultGoblin :: Unit
defaultGoblin = defaultElf {species = Goblin}

-- flip (i,j) for easier sorting
-- visits on (pos, start of path)
main :: IO ()
main = do
  contents <- readFile "data/2018-12-15.dat"
  let state = parse contents
      pt1 = battle state
      pt2 = invincible state
  putStrLn $ "Victory Score: " ++ show pt1
  putStrLn $ "Invincible Score: " ++ show pt2

invincible :: State -> Int
invincible state = score results
  where
    setElfAttack (cave, units) a = (cave, map (setElfAttack' a) units)
    setElfAttack' a u
      | species u == Elf = u {attack = a}
      | otherwise = u
    numElfs = length . filter ((== Elf) . species) . snd $ state
    -- empty list if an elf dies, else the victory state
    allElfsLive = (== numElfs) . length . filter ((== Elf) . species) . snd
    fight =
      dropWhile (not . victory . snd) .
      takeWhile (allElfsLive . snd) . zip [0 ..] . iterate update
    -- remove empty lists before taking the victory state
    results =
      map head . filter (not . null) . map fight . map (setElfAttack state) $
      [4 ..]

battle :: State -> Int
battle = score . dropWhile (not . victory . snd) . zip [0 ..] . iterate update

score :: [(Int, State)] -> Int
score states = hp * (rounds - 1)
  where
    (rounds, state) = head states
    hp = sum . map health . snd $ state

victory :: State -> Bool
victory = any victor . snd

update :: State -> State
update (cave, units) = (cave, L.sort . L.foldl' (updateUnit cave) units $ units)

updateUnit :: Cave -> [Unit] -> Unit -> [Unit]
updateUnit cave units c
  | null currents = units -- we've already killed this unit
  | null enemy = unitsV -- no enemies left
  | null combatants && null combatants' = units'
  | null combatants = combat units' current' (head combatants') -- move then fight
  | otherwise = combat units current (head combatants) -- just fight
  where
    currents = filter ((== position c) . position) $ units
    current = head currents
    enemy = filter ((/= species current) . species) units
    combatants =
      L.sortOn health . L.sort . filter ((<= 1) . udist current) $ enemy
    --
    current' = move cave units current
    combatants' =
      L.sortOn health . L.sort . filter ((<= 1) . udist current') $ enemy
    units' = current' : filter ((/= position current) . position) units
    --
    unitsV =
      current {victor = True} : filter ((/= position current) . position) units

combat :: [Unit] -> Unit -> Unit -> [Unit]
combat units friend foe
  | hp <= 0 = units'
  | otherwise = foe' : units'
  where
    hp = health foe - attack friend
    foe' = foe {health = hp}
    units' = filter ((/= position foe) . position) units

udist :: Unit -> Unit -> Int
udist u1 u2 = abs (i1 - i2) + abs (j1 - j2)
  where
    (i1, j1) = position u1
    (i2, j2) = position u2

move :: Cave -> [Unit] -> Unit -> Unit
move cave units current
  | J.isNothing next = current
  | null targets = current
  | otherwise = current'
  where
    allies =
      S.fromList . map position . filter ((== species current) . species) $
      units
    reachable = S.difference cave allies
    targets =
      filter ((/= species current) . species) >>>
      map (neighbours cave) >>>
      concat >>> map position >>> filter (`S.notMember` allies) >>> S.fromList $
      units
    -- find a path to the nearest enemy
    queue = Q.fromList [(0, current {path = []})]
    next = astar reachable targets M.empty queue []
    -- take the first step
    current' = current {position = head . path . J.fromJust $ next}

astar :: Cave -> Cave -> Visited -> Queue -> [Unit] -> Maybe Unit
astar cave targets visited q results
  | null q && null results = Nothing
  | null q = getResult results
  | null targets = Nothing
  | length results > 0 && estimate > (estimator . head $ results) =
    getResult results
  | S.member pos targets = astar cave targets visited queue (unit : results)
  | M.member v visited && visited M.! v <= estimate =
    astar cave targets visited queue results
  | otherwise = astar cave targets visited' queue' results
  where
    ((estimate, unit), queue) = Q.deleteFindMin q
    pos = position unit
    visitor = position &&& head . path
    v = visitor unit
    estimator = uncurry (+) . (length . path &&& estDist targets)
    visited' = M.insert v estimate visited
    nbhrs =
      neighbours cave >>>
      filter
        (\u ->
           let v = visitor u
            in (not $ M.member v visited') || visited' M.! v > estimator u) >>>
      map (estimator &&& id) >>> Q.fromList $
      unit
    queue' = Q.union queue nbhrs
    getResult = Just . head . L.sort

estDist :: Cave -> Unit -> Int
estDist cave unit =
  minimum .
  map
    (\(i, j) ->
       let (u, v) = position unit
        in abs (i - u) + abs (j - v)) .
  S.toList $
  cave

neighbours :: Cave -> Unit -> [Unit]
neighbours cave unit = nghbrs
  where
    pos = position unit
    ncoords =
      map (addC pos) >>> filter (`S.member` cave) $
      [(-1, 0), (0, -1), (0, 1), (1, 0)]
    addC (a, b) (c, d) = (a + c, b + d)
    nghbrs = map (\c -> unit {position = c, path = path unit ++ [c]}) $ ncoords

parse :: String -> State
parse input = (cave, units)
  where
    system =
      lines >>>
      zip [0 ..] >>>
      concatMap coordChar >>>
      filter (\(i, j, c) -> c /= ' ' && c /= '#') >>>
      map (\(i, j, c) -> ((i, j), c)) >>> M.fromList $
      input
    coordChar (j, line) = zipWith (\i char -> (j, i, char)) [0 ..] line
    cave = M.keysSet system
    units =
      M.filter (`L.elem` "EG") >>> M.toList >>> map toUnit >>> L.sort $ system

toUnit :: (Coord, C.Char) -> Unit
toUnit (coord, c)
  | c == 'E' = defaultElf {position = coord}
  | otherwise = defaultGoblin {position = coord}

view :: State -> String
view (cave, units) =
  "\n" ++ cavemap ++ "\nElves: " ++ show elfs ++ "\nGoblins: " ++ show goblins
  where
    us = M.fromList . map (position &&& species) $ units
    imx = maximum . map fst . S.toList $ cave
    jmx = maximum . map snd . S.toList $ cave
    toChr (i, j)
      | i == imx + 1 = '#'
      | i == imx + 2 = '\n'
      | j == jmx + 1 = '#'
      | M.member (i, j) us && us M.! (i, j) == Elf = 'E'
      | M.member (i, j) us && us M.! (i, j) == Goblin = 'G'
      | S.member (i, j) cave = '.'
      | otherwise = '#'
    cavemap = map toChr [(i, j) | j <- [0 .. jmx + 1], i <- [0 .. (imx + 2)]]
    elfs = map health . filter ((== Elf) . species) $ units
    goblins = map health . filter ((== Goblin) . species) $ units
{-
Combat ends after 37 full rounds
Elves win with 982 total hit points left
Outcome: 37 * 982 = 36334
-}
