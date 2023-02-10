{-# LANGUAGE TypeApplications #-}

import           Control.Arrow
import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Debug.Trace

type Sleep = M.Map Int Int

main :: IO ()
main = do
  contents <- readFile "data/2018-12-04.dat"
  -- contents <- readFile "data/test.dat"
  let entries = lines  >>> L.sort $ contents
      guard = sleepyGuard entries
      (mins, _) = sleepyTime guard entries
      part1 = guard * mins
  putStrLn $ "Part 1: " ++ show part1 ++ " " ++ show guard ++ " " ++ show mins

sleepyTime :: Int -> [String] -> (Int, Int) 
sleepyTime guard items = (mins, mx)
  where
    sleep = timesAsleep guard M.empty 0 0 items
    mx = maximum (M.elems sleep)
    (mins,_) = M.filter (>= mx) >>> M.findMin $ sleep


timesAsleep :: Int -> Sleep -> Int -> Int -> [String] -> Sleep
timesAsleep _ sleep _ _ [] = sleep
timesAsleep g sleep guard falls (item:items)
  | tokens L.!! 2 == "Guard" = timesAsleep g sleep guard' falls items
  | g /= guard = timesAsleep g sleep guard falls items
  | tokens L.!! 2 == "falls" = timesAsleep g sleep guard mins items
  | otherwise = timesAsleep g sleep' guard falls items
  where
    tokens = L.splitOn " " item
    guard' = drop 3 >>> head >>> filter (/= '#') >>> read @Int $ tokens
    mins =
      drop 1 >>>
      head >>>
      filter (/= ']') >>> L.splitOn ":" >>> drop 1 >>> head >>> read @Int $
      tokens
    sleep' = L.foldl' (\m t -> M.insertWith (+) t 1 m) sleep [falls..(mins-1)]

sleepyGuard :: [String] -> Int
sleepyGuard items = guard 
  where 
    sleep = timeSheet M.empty 0 0 items
    mx = maximum (M.elems sleep)
    (guard,_) = M.filter (>= mx) >>> M.findMin $ sleep

timeSheet :: Sleep -> Int -> Int -> [String] -> Sleep
timeSheet sleep _ _ [] = sleep
timeSheet sleep guard falls (item:items)
  | tokens L.!! 2 == "Guard" = timeSheet sleep guard' falls items
  | tokens L.!! 2 == "falls" = timeSheet sleep guard mins items
  | otherwise = timeSheet sleep' guard falls items
  where
    tokens = L.splitOn " " item
    guard' = drop 3 >>> head >>> filter (/= '#') >>> read @Int $ tokens
    mins =
      drop 1 >>>
      head >>>
      filter (/= ']') >>> L.splitOn ":" >>> drop 1 >>> head >>> read @Int $
      tokens
    sleep' = M.insertWith (+) guard (mins - falls) sleep
