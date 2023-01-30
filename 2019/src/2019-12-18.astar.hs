{-# LANGUAGE BlockArguments #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow
import qualified Data.Char            as C
import qualified Data.List            as L
import           Data.List.Extra      as E
import qualified Data.Map             as M
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Set             as S
import           Debug.Trace

type Coord = (Int, Int)

type Tunnels = M.Map Coord Char

type Keys = S.Set C.Char

type Visited = M.Map ([Coord], Keys) Int

type Queue = Q.MinPQueue Int State

data State =
  State
    { position :: [Coord]
    , keys     :: Keys
    , distance :: Int
    , active   :: Int
    , newKey :: Bool 
    }
  deriving (Show, Eq)

main :: IO ()
main = do
  contents <- readFile "data/2019-12-18.dat"
  -- contents <- readFile "data/test.dat"
  let tunnels = parse contents
      part1 = solve tunnels
      tunnels' = updateTunnels tunnels
      part4 = solve tunnels'
  putStrLn $ "distance: " ++ show (distance part1) 
  putStrLn $ "distance, multi-robot: " ++ show (distance part4)

solve :: Tunnels -> State
solve tunnels = astar tunnels estDFn haltFn visited queue
  where
    allSymbols = M.elems >>> S.fromList $ tunnels
    ks = S.filter (\c -> not $ C.isUpper c) allSymbols
    visited = M.empty :: Visited
    start = starts tunnels
    state = State {position = start, keys = ks, distance = 0, active = 0, newKey=False}
    queue = Q.singleton (estDFn state) state
    keyPos = M.filter (\k -> C.isLower k) >>> M.toList >>> map (\(a,b) -> (b,a)) >>> M.fromList $ tunnels 
    estDFn = estimateDist keyPos 
    ksLower = S.filter (\c -> C.isLower c) allSymbols
    haltFn = halt (length ks + length ksLower)

halt :: Int -> State -> Bool 
halt target = (== target) . length . keys 

estimateKeys :: Int -> State -> Int
estimateKeys target s = distance s + target - length (keys s)

estimateDist :: M.Map Char Coord -> State -> Int
estimateDist keyPos s = distance s + keyDist + length remaining
  where
    ks = keys s
    remaining = S.filter (\k -> S.notMember (C.toUpper k) ks) ks
    keyDist = S.map (\k -> minDist (position s) (keyPos M.! k)) >>> maximum $ remaining

minDist :: [Coord] -> Coord -> Int 
minDist ps (i,j) = map (\(pi,pj) -> abs (pi-i) + abs (pj-j))  >>> minimum $ ps

updateTunnels :: Tunnels -> Tunnels
updateTunnels tunnels = tunnels''
  where
    (i, j) = head $ starts tunnels
    toPts = map (\(di, dj) -> (i + di, j + dj))
    tunnels' =
      toPts >>> foldr (\c t -> M.insert c '@' t) tunnels $
      [(-1, -1), (-1, 1), (1, -1), (1, 1)]
    tunnels'' =
      toPts >>> foldr (\c t -> M.delete c t) tunnels' $
      [(0, -1), (-1, 0), (0, 0), (1, 0), (0, 1)]

starts :: Tunnels -> [Coord]
starts = M.filter (== '@') >>> M.toList >>> map fst

astar :: Tunnels -> (State -> Int) -> (State -> Bool) -> Visited -> Queue -> State
astar tunnels estFn haltFn visited q
  | haltFn state' = state'
  | M.member (pos, ks') visited && visited M.! (pos, ks') <= d =
    astar tunnels estFn haltFn visited queue
  | null queue' = state
  | otherwise = astar tunnels estFn haltFn visited' queue'
  where
    ((_, state), queue) = Q.deleteFindMin q
    pos = position state
    ks = keys state
    d = distance state
    visited' = M.insert (pos, ks') d visited
    ks' =
      map (\p -> tunnels M.! p) >>>
      filter C.isLower >>> map C.toUpper >>> foldr (\l k -> S.insert l k) ks $
      pos
    state' = state {keys = ks', newKey = ks /= ks'}
    nbhrs =
      neighbours tunnels >>>
      filter
        (\s ->
           let p = position s
               ks = keys s
               d = distance s
            in (not $ M.member (p, ks) visited') || visited' M.! (p, ks) > d) >>>
      map (\s -> (estFn s, s)) >>> Q.fromList $
      state'
    queue' = Q.union queue nbhrs

{- pne active robot at a time, apart frpm at home, keys and locks -}
neighbours :: Tunnels -> State -> [State]
neighbours tunnels state
  | distance state == 0 = concatRobotNhbrs [0 .. (l - 1)]
  | newKey state = concatRobotNhbrs [0 .. (l - 1)]
  | otherwise = concatRobotNhbrs [active state]
  where
    l = length $ position state
    concatRobotNhbrs = concat . map (neighbourRobot tunnels state)

neighbourRobot :: Tunnels -> State -> Int -> [State]
neighbourRobot tunnels state num = nghbrs
  where
    pos = position state
    (i, j) = pos !! num
    ks = keys state
    d = distance state
    ncoords =
      map (\(di, dj) -> (i + di, j + dj)) >>>
      filter (`M.member` tunnels) >>> filter (\c -> S.member (tunnels M.! c) ks) $
      [(0, 1), (0, -1), (1, 0), (-1, 0)]
    nghbrs =
      map
        (\c ->
           state
             { distance = 1 + d
             , position = (take num pos) ++ [c] ++ (drop (num + 1) pos)
             , active = num
             })
        ncoords

parse :: String -> M.Map Coord C.Char
parse =
  lines >>>
  zip [0 ..] >>>
  concatMap coordChar >>> filter (\(coord, chr) -> chr /= '#') >>> M.fromList

coordChar :: (Int, String) -> [(Coord, Char)]
coordChar (j, line) = zipWith (\i char -> ((i, j), char)) [0 ..] line
{-

#############
#g#f.D#..h#l#
#F###e#E###.#
#dCba...BcIJ#
#####.@.#####
#nK.L...G...#
#M###N#H###.#
#o#m..#i#jk.#
#############

distance: 114
distance, multi-robot: 72

lists:
real    0m1.245s
user    0m1.207s
sys     0m0.030s

astar, dist - #keys:
real    0m0.216s
user    0m0.183s
sys     0m0.029s


2019-12-18.dat
distance: 4668 (list)

real    0m29.005s
user    0m28.669s
sys     0m0.267s

distance: 4668 (sequence)
real    0m29.016s
user    0m28.648s
sys     0m0.294s

astar : bfs
real    0m33.705s
user    0m33.251s
sys     0m0.459s

astar, dist - #keys:
real    0m24.716s
user    0m24.291s
sys     0m0.457s

distance: 4668
distance, multi-robot: 1910

astar, dist + keydist - #keys:
real    21m52.478s
user    20m29.648s
sys     0m15.061s

astar, branch on new key, single active robot:
real    2m9.897s
user    2m8.124s
sys     0m1.459s
-}
