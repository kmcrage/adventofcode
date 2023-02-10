{-# LANGUAGE TypeApplications #-}

import           Control.Arrow
import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Debug.Trace

-- (guard, minute) = count
type Sleep = M.Map (Int, Int) Int

main :: IO ()
main = do
  contents <- readFile "data/2018-12-04.dat"
  --contents <- readFile "data/test.dat"
  let sleep = parse contents
      (guard, mn) = sleepyGuard sleep
      part1 = guard * mn
      (_, guard2, min2) = maxSleep sleep
      part2 = guard2 * min2
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2

maxSleep :: Sleep -> (Int, Int, Int)
maxSleep = M.toList >>> map (\((g, m), c) -> (c, g, m)) >>> L.sort >>> last

sleepyGuard :: Sleep -> (Int, Int)
sleepyGuard sleep = (guard, mn)
  where
    gSleep =
      M.foldrWithKey (\(g, _) c gs -> M.insertWith (+) g c gs) M.empty sleep
    mx = M.elems >>> maximum $ gSleep
    guard = M.filter (>= mx) >>> M.findMax >>> fst $ gSleep
    minSleep = M.filterWithKey (\(g, _) _ -> g == guard) sleep
    mx' = M.elems >>> maximum $ minSleep
    mn = M.filter (>= mx') >>> M.findMax >>> fst >>> snd $ minSleep



parse :: String -> Sleep
parse contents = parseLines M.empty 0 0 items
  where
    items = lines >>> L.sort $ contents

parseLines :: Sleep -> Int -> Int -> [String] -> Sleep
parseLines sleep _ _ [] = sleep
parseLines sleep guard falls (item:items)
  | tokens L.!! 2 == "Guard" = parseLines sleep guard' falls items
  | tokens L.!! 2 == "falls" = parseLines sleep guard mins items
  | otherwise = parseLines sleep' guard falls items
  where
    tokens = L.splitOn " " item
    guard' = drop 3 >>> head >>> filter (/= '#') >>> read @Int $ tokens
    mins =
      drop 1 >>>
      head >>>
      filter (/= ']') >>> L.splitOn ":" >>> drop 1 >>> head >>> read @Int $
      tokens
    sleep' =
      L.foldl'
        (\m t -> M.insertWith (+) (guard, t) 1 m)
        sleep
        [falls .. (mins - 1)]
