import           Data.List.Split

import           IntCode

main :: IO ()
main = do
  contents <- readFile "data/2019-12-05.dat"
    -- contents <- readFile "data/test.dat"
  let intcodes = map read_int strcodes
      read_int n = read n :: Int
      strcodes = splitOn "," contents
      part_one = runPrg intcodes [1]
      diagnostics_one = output part_one
      result_one = last diagnostics_one
      part_two = runPrg intcodes [5]
      result_two = last $ output part_two
  putStrLn $ "Diagnostics One: " ++ show diagnostics_one
  putStrLn $ "Result One: " ++ show result_one
  putStrLn $ "Diagnostics Two: " ++ show result_two
