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
order prod = search prod 0 [] S.empty queue "" 
  where
    (provides, requires,_,_) = prod
    starts = S.difference (M.keysSet provides) (M.keysSet requires)
    queue = S.toList >>> map (id &&& id) >>> Q.fromList $ starts

search ::
  Production
  -> Int
  -> [Int]
  -> S.Set String
  -> Q.MinPQueue String String
  -> String
  -> (Int, String)
search prod time elves found queue result
  | Q.null queue = (time, result) -- empty queue, all processed
  | S.member step found = search prod time elves found queue'' result -- already processed 
  | S.isSubsetOf (M.findWithDefault S.empty step requires) found =
    search prod time elves found' queue' result' -- process
  | otherwise = search prod time elves found queue'' result -- unforfilled prerequisites
  where
    (provides, requires, workers, duration) = prod 
    ((_, step), queue'') = Q.deleteFindMin queue
    result' = result ++ step
    found' = S.insert step found
    queue' =
      S.toList >>> L.foldl' (\q s -> Q.insert s s q) queue $
      M.findWithDefault S.empty step provides

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
