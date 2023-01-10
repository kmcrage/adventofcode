import           Data.List (group, sort)

main :: IO ()
main = do
  putStrLn $ "part 1: " ++ show result_1
  putStrLn $ "part 2: " ++ show result_2
  where
    ordered_nums = filter ordered [171309 .. 643603]
    result_1 = length $ filter doubled ordered_nums
    result_2 = length $ filter strictlyDoubled ordered_nums

ordered :: Int -> Bool
ordered n = show n == sort (show n)

doubled :: Int -> Bool
doubled = any ((>= 2) . length) . group . show

strictlyDoubled :: Int -> Bool
strictlyDoubled = any ((== 2) . length) . group . show
