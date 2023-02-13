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

type Requires = M.Map C.Char (S.Set C.Char)

data Production =
  Production
    { provides :: Requires
    , requires :: Requires
    , workers  :: Int
    , duration :: Int
    }

data State =
  State
    { time   :: Int
    , found  :: S.Set C.Char
    , elves  :: Q.MinPQueue Int C.Char -- time, char
    , queue  :: Q.MinPQueue C.Char C.Char
    , result :: String
    }

main :: IO ()
main = do
  contents <- readFile "data/2018-12-07.dat"
  -- contents <- readFile "data/test.dat"
  let (prov, reqs) = parse contents
      prod =
        Production {provides = prov, requires = reqs, workers = 1, duration = 0}
      part1 = order prod
      -- part2 = order prod {workers=2} -- test
      part2 = order prod {workers = 5, duration = 60}
  putStrLn $ "Part 1, 1 worker, 0s delay: " ++ show part1
  putStrLn $ "Part 2, 5 workers, 60s delay: " ++ show part2

order :: Production -> (Int, String)
order prod = search prod state
  where
    prov = provides prod
    reqs = requires prod
    starts = S.difference (M.keysSet prov) (M.keysSet reqs)
    queue = S.toList >>> map (id &&& id) >>> Q.fromList $ starts
    state =
      State
        {time = 0, elves = Q.empty, found = S.empty, queue = queue, result = ""}

search :: Production -> State -> (Int, String)
search prod state
  | null elfs && Q.null que = (tm, rslt) -- empty queues, all processed
  | Q.null que = processElf prod state
  | length elfs == wrkrs = processElf prod state
  | otherwise = processQueue prod state
  where
    wrkrs = workers prod
    que = queue state
    elfs = elves state
    tm = time state
    rslt = result state

processQueue :: Production -> State -> (Int, String)
processQueue prod state
  | S.member step fnd = search prod state' -- already processed
  | hasReqs = search prod stateE -- elf can start work
  | otherwise = search prod state'
  where
    reqs = requires prod
    durn = duration prod
    ((_, step), que) = Q.deleteFindMin $ queue state
    fnd = found state
    --
    state' = state {queue = que}
    --
    hasReqs = S.isSubsetOf (M.findWithDefault S.empty step reqs) fnd
    elves' = elves >>> Q.insert (durn + C.ord step - 64) step $ state
    stateE = state' {elves = elves'}

processElf :: Production -> State -> (Int, String)
processElf prod state
  | S.member step fnd = search prod state' -- already processed
  | otherwise = search prod stateQ
  where
    prov = provides prod
    ((t, step), elfs) = Q.deleteFindMin $ elves state
    que = queue state
    fnd = found state
    --
    time' = t + time state
    elfs' = Q.mapKeysMonotonic (subtract t) elfs
    state' = state {elves = elfs', time = time'}
    --
    fnd' = S.insert step fnd
    rslt = result state ++ [step]
    que' =
      S.toList >>> L.foldr (\s -> Q.insert s s) que $
      M.findWithDefault S.empty step prov
    stateQ = state' {queue = que', result = rslt, found = fnd'}

parse :: String -> (Requires, Requires)
parse input = (provides, requires)
  where
    pairs = lines >>> map parseLine $ input
    flip = map (\(a, b) -> (b, a))
    process =
      L.foldr (\(p, r) -> M.insertWith (S.union) p (S.singleton r)) M.empty
    provides = process pairs
    requires = process . flip $ pairs

parseLine :: String -> (C.Char, C.Char)
parseLine line = (pre, post)
  where
    tokens = words line
    pre = head $ tokens !! 1
    post = head $ tokens !! 7
