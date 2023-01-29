{-# LANGUAGE BlockArguments #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow
import qualified Data.Char       as C
import qualified Data.List       as L
import           Data.List.Extra as E
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Debug.Trace

type Coord = (Int, Int)

type Tunnels = M.Map Coord Char

type Keys = S.Set C.Char

type Visited = S.Set ([Coord], Keys)

data State =
  State
    { position :: [Coord]
    , keys     :: Keys
    , distance :: Int
    }
  deriving (Show, Eq)

main :: IO ()
main
  -- contents <- readFile "data/2019-12-18.dat"
 = do
  contents <- readFile "data/test.dat"
  let tunnels = parse contents
      part1 = solve tunnels
      tunnels' = updateTunnels tunnels
      part4 = solve tunnels'
  putStrLn $ "distance: " ++ show (distance part1)
  putStrLn $ "distance, multi-robot: " ++ show (distance part4)

solve :: Tunnels -> State
solve tunnels = bfs tunnels target visited [state]
  where
    allSymbols = M.elems >>> S.fromList $ tunnels
    ks = S.filter (\c -> not $ C.isUpper c) allSymbols
    target = length ks + (length $ S.filter (C.isLower) allSymbols)
    visited = S.empty :: Visited
    start = starts tunnels
    state = State {position = start, keys = ks, distance = 0}

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

bfs :: Tunnels -> Int -> Visited -> [State] -> State
bfs tunnels target visited (state:queue)
  | (>= target) . length . keys $ state' = state'
  | S.member (pos, ks') visited = bfs tunnels target visited queue
  | null queue' = state
  | otherwise = bfs tunnels target visited' queue'
  where
    pos = position state
    ks = keys state
    visited' = S.insert (pos, ks') visited
    ks' =
      map (\p -> C.toUpper $ tunnels M.! p) >>> foldr (\l k -> S.insert l k) ks $
      pos
    state' = state {keys = ks'}
    nbhrs =
      filter
        (\s ->
           let p = position s
               ks = keys s
            in not $ S.member (p, ks) visited') $
      neighbours tunnels state'
    queue' = queue ++ nbhrs

neighbours :: Tunnels -> State -> [State]
neighbours tunnels state =
  concat . map (\i -> neighbourRobot i tunnels state) $ [0 .. (l - 1)]
  where
    l = length $ position state

neighbourRobot :: Int -> Tunnels -> State -> [State]
neighbourRobot num tunnels state = nghbrs
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

real    0m1.245s
user    0m1.207s
sys     0m0.030s


2019-12-18.dat

-}
