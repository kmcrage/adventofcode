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
chainIsDone = all ((== Halted) . state)

chaineval :: [Int] -> Int -> [Int] -> [Int]
chaineval = chainevalFn runChain

chainevalOnce :: [Int] -> Int -> [Int] -> [Int]
chainevalOnce = chainevalFn runChainOnce

chainevalFn :: ([Machine] -> [Machine]) -> [Int] -> Int -> [Int] -> [Int]
chainevalFn fn intcode signal phases = output finalm
  where
    chain = map (initMachine intcode . singleton) phases
    chain' = addSignalChain chain [signal]
    resultm = fn chain'
    finalm = last resultm

addSignalChain :: [Machine] -> [Int] -> [Machine]
addSignalChain (m:machines) signal = n : machines
  where
    n = addSignal m signal

runChain :: [Machine] -> [Machine]
runChain ms
  | chainIsDone ns = ns
  | otherwise = runChain ns'
  where
    ns = runChainOnce ms
    ns' = feedbackSignal ns

feedbackSignal :: [Machine] -> [Machine]
feedbackSignal ms
  | chainIsDone ms = ms
  | otherwise = start ++ [l']
  where
    l = last ms
    ns = addSignalChain ms (output l)
    start = take (length ns - 1) ns
    l' = flushOutput l

runChainOnce :: [Machine] -> [Machine]
runChainOnce (m:ms)
  | null ms = [m']
  | otherwise = n : runChainOnce ns
  where
    m' = runPrgAt m
    n = flushOutput m'
    ns = addSignalChain ms (output m')
