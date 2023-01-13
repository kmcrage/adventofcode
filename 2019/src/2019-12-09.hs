import           Data.List.Split

import           IntCode

main :: IO ()
main = do
  contents <- readFile "data/2019-12-09.dat"
  -- contents <- readFile "data/test.dat"
  let intcodes = map read_int strcodes
      read_int n = read n :: Int
      strcodes = splitOn "," contents
      intcodes' = intcodes ++ repeat 0
      diagnostic = outputM $ runPrg intcodes' [1]
      result = outputM $ runPrg intcodes' [2]
  putStrLn $ "Part One Output: " ++ show diagnostic
  putStrLn $ "Part Two Output: " ++ show result