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

type Visited = S.Set (Coord, Keys)

data State =
  State
    { position :: [Coord]
    , keys     :: Keys
    , distance :: Int
    }
  deriving (Show, Eq)

main :: IO ()
main = do
  contents <- readFile "data/2019-12-18.dat"
  -- contents <- readFile "data/test.dat"
  let tunnels = parse contents
      target = 2 + (length . M.filter (>= 'a') $ tunnels)
      start = starts tunnels
      keys = S.fromList ['.', '@']
      visited = S.empty :: Visited
      state = State {position = start, keys = keys, distance = 0}
      path = distance $ bfs tunnels target visited [state]
      tunnelsPart2 = updateTunnels tunnels
      startPart2 = starts tunnelsPart2
      statePart2 = State {position = startPart2, keys = keys, distance = 0}
      pathPart2 = distance $ bfs tunnelsPart2 target visited [statePart2]
  putStrLn $ "distance: " ++ show path
  putStrLn $ "distance, multi-robot: " ++ show pathPart2

updateTunnels :: Tunnels -> Tunnels
updateTunnels tunnels = tunnels''
  where
    (i, j) = head $ starts tunnels
    tunnels' =
      map (\(di, dj) -> (i + di, j + dj)) >>>
      foldr (\c t -> M.insert c '@' t) tunnels $
      [(-1, -1), (-1, 1), (1, -1), (1, 1)]
    tunnels'' =
      map (\(di, dj) -> (i + di, j + dj)) >>>
      foldr (\c t -> M.delete c t) tunnels' $
      [(0, -1), (-1, 0), (0, 0), (1, 0), (0, 1)]

starts :: Tunnels -> [Coord]
starts = M.filter (== '@') >>> M.toList >>> map fst

bfs :: Tunnels -> Int -> Visited -> [State] -> State
bfs tunnels target visited (state:queue)
  | (== target) . length . keys $ state' = state' -- this is for part one
  | L.all (\p -> S.member (p, ks) visited) pos =
    bfs tunnels target visited queue
  | null queue' = state -- this is the case where we search exhaustively i.e. part two
  | otherwise = bfs tunnels target visited' queue'
  where
    pos = position state
    ks = keys state
    visited' = foldr (\p v -> S.insert (p, ks) v) visited pos
    -- visited' = S.foldr (\s v -> S.insert (pos, s) v) visited $ S.powerSet ks
    locks = map (\p -> C.toUpper $ tunnels M.! p) pos
    ks' = foldr (\l k -> S.insert l k) ks locks
    state' = state {keys = ks'}
    queue' = queue ++ neighbours tunnels state'

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
      filter (`M.member` tunnels) >>>
      filter
        (\c ->
           let t = tunnels M.! c
            in t > 'Z' || S.member t ks) $
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
