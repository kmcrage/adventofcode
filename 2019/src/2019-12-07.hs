import           Data.List
import           Data.List.Split

import           IntCode

main :: IO ()
main = do
  contents <- readFile "data/2019-12-07.dat"
  -- contents <- readFile "data/test.dat"
  let intcode = map read_int strcodes
      read_int n = read n :: Int
      strcodes = splitOn "," contents
      part_one = chainmaxOnce intcode 0 [0 .. 4]
      part_two = chainmax intcode 0 [5 .. 9]
  putStrLn $ "Max Signal: " ++ show part_one
  putStrLn $ "Max Feedback Signal: " ++ show part_two

chainmaxOnce :: [Int] -> Int -> [Int] -> Int
chainmaxOnce = chainmaxFn chainevalOnce

chainmax :: [Int] -> Int -> [Int] -> Int
chainmax = chainmaxFn chaineval

chainmaxFn :: ([Int] -> Int -> [Int] -> [Int]) -> [Int] -> Int -> [Int] -> Int
chainmaxFn fn intcode init phases = maximum $ map (maximum . chainop) perms
  where
    chainop = fn intcode init
    perms = permutations phases

chainIsDone :: [Machine] -> Bool
chainIsDone = all ((== Halted) . stateM)

chaineval :: [Int] -> Int -> [Int] -> [Int]
chaineval = chainevalFn runchain

chainevalOnce :: [Int] -> Int -> [Int] -> [Int]
chainevalOnce = chainevalFn runchainOnce

chainevalFn :: ([Machine] -> [Machine]) -> [Int] -> Int -> [Int] -> [Int]
chainevalFn fn intcode signal phases = outputM finalm
  where
    chain = map (initMachine intcode) phases
    chain' = addSignal chain [signal]
    resultm = fn chain'
    finalm = last resultm

addSignal :: [Machine] -> [Int] -> [Machine]
addSignal (m:machines) signal = n : machines
  where
    n = addSignalM m signal

runchain :: [Machine] -> [Machine]
runchain ms
  | chainIsDone ns = ns
  | otherwise = runchain ns'
  where
    ns = runchainOnce ms
    ns' = feedbackSignal ns

feedbackSignal :: [Machine] -> [Machine]
feedbackSignal ms
  | chainIsDone ms = ms
  | otherwise = start ++ [l']
  where
    l = last ms
    ns = addSignal ms (outputM l)
    start = take (length ns - 1) ns
    l' = flushOutput l

runchainOnce :: [Machine] -> [Machine]
runchainOnce (m:ms)
  | null ms = [m']
  | otherwise = n : runchainOnce ns
  where
    m' = runPrgAt m
    n = flushOutput m'
    ns = addSignal ms (outputM m')
