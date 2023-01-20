import           Data.List.Split

import           IntCode

main :: IO ()
main = do
  contents <- readFile "data/2019-12-09.dat"
  -- contents <- readFile "data/test.dat"
  let intcodes = readIntcode contents
      diagnostic = head . output $ runPrg intcodes [1]
      result = head . output $ runPrg intcodes [2]
  putStrLn $ "Part One Output: " ++ show diagnostic
  putStrLn $ "Part Two Output: " ++ show result
