import           Data.List.Split

import           IntCode

main :: IO ()
main = do
  contents <- readFile "data/2019-12-02.dat"
    -- contents <- readFile "data/test.dat"
  let intcodes = map read_int strcodes
      read_int n = read n :: Int
      strcodes = splitOn "," contents
      intcodes' = replaceElementAt intcodes 1 12
      intcodes'' = replaceElementAt intcodes' 2 2
      result = head . memory $ runPrg intcodes'' []
      solution = findNounVerb intcodes 19690720
      nounVerb = head solution * 100 + (solution !! 1)
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Noun, verb: " ++ show nounVerb

findNounVerb :: [Int] -> Int -> [Int]
findNounVerb xs ans = findNounVerbIter xs ans 0 0

findNounVerbIter :: [Int] -> Int -> Int -> Int -> [Int]
findNounVerbIter xs ans noun verb
  | memory0 == ans = [noun, verb]
  | noun < 99 = findNounVerbIter xs ans (noun + 1) verb
  | otherwise = findNounVerbIter xs ans 0 (verb + 1)
  where
    xsn = replaceElementAt xs 1 noun
    xsnv = replaceElementAt xsn 2 verb
    memory0 = head . memory $ runPrg xsnv []
