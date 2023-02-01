{-# LANGUAGE BlockArguments #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow
import qualified Data.Char            as C
import qualified Data.List            as L
import qualified Data.Map             as M
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Set             as S
import           Debug.Trace

type Coord = (Int, Int)

type Tunnels = M.Map Coord Char

type Teleports = M.Map Coord (Coord, Int)

type Visited = M.Map (Coord, Int) Int

type Queue = Q.MinPQueue Int State

data Maze =
  Maze
    { start     :: Coord
    , end       :: Coord
    , tunnels   :: Tunnels
    , teleports :: Teleports
    , levels    :: Bool
    }
  deriving (Show, Eq)

data State =
  State
    { position :: Coord
    , distance :: Int
    , level    :: Int
    }
  deriving (Show, Eq)

main :: IO ()
main
 = do
  contents <- readFile "data/2019-12-20.dat"
  -- contents <- readFile "data/test.dat"
  let maze = parse contents
      part1 = solve maze
      part2 = solve maze {levels = True}
  putStrLn $ "part1, distance: " ++ show (distance part1)
  putStrLn $ "part2, distance: " ++ show (distance part2)

solve :: Maze -> State
solve maze = astar maze estFn haltFn visited queue
  where
    visited = M.empty :: Visited
    state = State {position = start maze, distance = 0, level = 0}
    queue = Q.singleton (estFn state) state
    estFn = estimate maze
    haltFn = halt maze

halt :: Maze -> State -> Bool
halt maze state = (end maze == position state) && (level state == 0)

{-
this estimate could be improved by computing manhattan distances
between teleports, then using floyd-warshall to compute under-estimates
for distance to goal (minus 1 for the "free" level change)
-}
estimate :: Maze -> State -> Int
estimate _ state = (distance state) + (level state)


astar :: Maze -> (State -> Int) -> (State -> Bool) -> Visited -> Queue -> State
astar maze estFn haltFn visited q
  | haltFn state = state
  | M.member (pos, l) visited && visited M.! (pos, l) <= d =
    astar maze estFn haltFn visited queue
  | null queue' = state
  | otherwise = astar maze estFn haltFn visited' queue'
  where
    ((_, state), queue) = Q.deleteFindMin q
    pos = position state
    l = level state
    d = distance state
    visited' = M.insert (pos, l) d visited
    nbhrs =
      neighbours maze >>>
      filter
        (\s ->
           let p = position s
               l = level s
               d = distance s
            in (not $ M.member (p, l) visited') || visited' M.! (p, l) > d) >>>
      map (\s -> (estFn s, s)) >>> Q.fromList $
      state
    queue' = Q.union queue nbhrs

neighbours :: Maze -> State -> [State]
neighbours maze state
  | levels maze = nghbrs ++ tghbrs'
  | otherwise = nghbrs ++ tghbrs
  where
    pos = position state
    d = distance state
    l = level state
    ts = tunnels maze
    tps = teleports maze
    tcoords = filter (`M.member` tps) >>> map (tps M.!) $ [pos]
    ncoords =
      map (addC pos) >>> filter (`M.member` ts) $
      [(0, 1), (0, -1), (1, 0), (-1, 0)]
    nghbrs = map (\c -> state {distance = 1 + d, position = c}) $ ncoords
    tghbrs = map (\(c, _) -> state {distance = 1 + d, position = c}) $ tcoords
    tghbrs' =
      map (\(c, dl) -> state {distance = 1 + d, position = c, level = l + dl}) >>>
      filter (\s -> level s >= 0) $
      tcoords

parse :: String -> Maze
parse input =
  Maze {tunnels = ts, start = s, end = e, teleports = tps, levels = False}
  where
    tokens =
      lines >>>
      zip [0 ..] >>>
      concatMap coordChar >>>
      filter (\(_, chr) -> chr /= '#' && chr /= ' ') >>> M.fromList $
      input
    ts = M.filter (not . C.isUpper) tokens
    labels = M.filter C.isUpper >>> M.mapWithKey (doubleLabel tokens) $ tokens
    coordLabels = M.mapMaybeWithKey (toCoordLabel labels) $ tokens
    (s, _) = M.findMin $ M.filter (== "AA") coordLabels
    (e, _) = M.findMin $ M.filter (== "ZZ") coordLabels
    ((mni, mnj), _) = M.findMin $ tokens
    ((mxi, mxj), _) = M.findMax $ tokens
    centre = (div (mni + mxi) 2, div (mnj + mxj) 2)
    tps = 
      map (findTeleport coordLabels centre) >>> foldr (M.union) M.empty $
      M.elems labels

toCoordLabel :: M.Map Coord String -> Coord -> Char -> Maybe String
toCoordLabel labels coord chr
  | chr == '#' = Nothing
  | chr == ' ' = Nothing
  | chr /= '.' = Nothing
  | otherwise = Just result
  where
    result =
      map (addC coord) >>> map (\c -> M.findWithDefault "" c labels) >>> concat $
      [(0, -1), (-1, 0), (0, 0), (1, 0), (0, 1)]

findTeleport :: M.Map Coord String -> Coord -> String -> Teleports
findTeleport labels centre label = tps
  where
    coordLabel = M.filter (== label) labels
    mn = fst $ M.findMin coordLabel
    mx = fst $ M.findMax coordLabel
    dl = dLevel centre mn mx
    tps =
      filter (\(a, (b, _)) -> a /= b) >>> M.fromList $
      [(mn, (mx, dl)), (mx, (mn, negate dl))]

dLevel :: Coord -> Coord -> Coord -> Int
dLevel (ci, cj) (ai, aj) (bi, bj)
  | distCA < distCB = 1
  | otherwise = (-1)
  where
    distCA = maximum [abs (ci - ai), abs (cj - aj)]
    distCB = maximum [abs (ci - bi), abs (cj - bj)]

doubleLabel :: Tunnels -> Coord -> Char -> String
doubleLabel tunnels coord _ = label
  where
    label =
      map (addC coord) >>>
      map (\p -> M.findWithDefault '.' p tunnels) >>>
      filter (/= ' ') >>> filter (/= '.') $
      [(0, -1), (-1, 0), (0, 0), (1, 0), (0, 1)]

addC :: Coord -> Coord -> Coord
addC (a, b) (c, d) = (a + c, b + d)

coordChar :: (Int, String) -> [(Coord, Char)]
coordChar (j, line) = zipWith (\i char -> ((i, j), char)) [0 ..] line
