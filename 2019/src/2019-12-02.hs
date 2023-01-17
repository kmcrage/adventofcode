import           Data.List.Split
import qualified Data.Map        as M

import           IntCode

main :: IO ()
main = do
  contents <- readFile "data/2019-12-02.dat"
    -- contents <- readFile "data/test.dat"
  let intcodes = map read_int strcodes
      read_int n = read n :: Int
      strcodes = splitOn "," contents
      intcodes' = replaceNounVerb intcodes [12, 2]
      result = memory0 $ runPrg intcodes' []
      solution = findNounVerb intcodes 19690720
      nounVerb = head solution * 100 + (solution !! 1)
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Noun, verb: " ++ show nounVerb

replaceNounVerb :: [Int] -> [Int] -> [Int]
replaceNounVerb xs ins = x : ins ++ post
  where
    x = head xs
    post = drop 3 xs

findNounVerb :: [Int] -> Int -> [Int]
findNounVerb xs ans = findNounVerbIter xs ans 0 0

findNounVerbIter :: [Int] -> Int -> Int -> Int -> [Int]
findNounVerbIter xs ans noun verb
  | mem0 == ans = [noun, verb]
  | noun < 99 = findNounVerbIter xs ans (noun + 1) verb
  | otherwise = findNounVerbIter xs ans 0 (verb + 1)
  where
    xsnv = replaceNounVerb xs [noun, verb]
    mem0 = memory0 $ runPrg xsnv []
