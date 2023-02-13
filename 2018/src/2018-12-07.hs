{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow
import qualified Data.Char            as C
import qualified Data.List            as L
import qualified Data.List.Split      as L
import qualified Data.Map             as M
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Set             as S
import           Debug.Trace

type Requires = M.Map String (S.Set String)

type Production = (Requires, Requires, Int, Int)

data State =
  State
    { time   :: Int
    , elves  :: [Int]
    , found  :: S.Set String
    , queue  :: Q.MinPQueue String String
    , result :: String
    }

main :: IO ()
main = do
  contents <- readFile "data/2018-12-07.dat"
  --contents <- readFile "data/test.dat"
  let (provides, requires) = parse contents
      part1 = order (provides, requires, 1, 0)
      part2 = order (provides, requires, 2, 0) -- test
      -- part2 = order (provides, requires, 5, 60)
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2

order :: Production -> (Int, String)
order prod = search prod state
  where
    (provides, requires, _, _) = prod
    starts = S.difference (M.keysSet provides) (M.keysSet requires)
    queue = S.toList >>> map (id &&& id) >>> Q.fromList $ starts
    state =
      State {time = 0, elves = [], found = S.empty, queue = queue, result = ""}

search :: Production -> State -> (Int, String)
search prod state
  | Q.null que = (tm, rslt) -- empty queue, all processed
  | S.member step fnd = search prod state'' -- already processed
  | hasReqs fnd = search prod state' -- process
  | otherwise = search prod state'' -- unforfiled prerequisites
  where
    (provides, requires, workers, duration) = prod
    que = queue state
    fnd = found state
    tm = time state
    rslt = result state
    ((_, step), que'') = Q.deleteFindMin que
    hasReqs =  S.isSubsetOf $ M.findWithDefault S.empty step requires
    rslt' = rslt ++ step
    fnd' = S.insert step fnd
    que' =
      S.toList >>> L.foldl' (\q s -> Q.insert s s q) que $
      M.findWithDefault S.empty step provides
    state'' = state {queue = que''}
    state' = state {found = fnd', queue = que', result = rslt'}

parse :: String -> (Requires, Requires)
parse input = (provides, requires)
  where
    pairs = lines >>> map parseLine $ input
    provides =
      L.foldr
        (\(p, r) -> M.insertWith (S.union) p (S.singleton r))
        M.empty
        pairs
    requires =
      L.foldr
        (\(p, r) -> M.insertWith (S.union) r (S.singleton p))
        M.empty
        pairs

parseLine :: String -> (String, String)
parseLine line = (pre, post)
  where
    tokens = words line
    pre = tokens !! 1
    post = tokens !! 7
