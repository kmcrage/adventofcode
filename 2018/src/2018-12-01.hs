import           Control.Arrow
import qualified Data.List     as L
import qualified Data.Set      as S
import           Debug.Trace

main :: IO ()
main = do
  contents <- readFile "data/2018-12-01.dat"
    -- contents <- readFile "data/test.dat"
  let nums = lines >>> map (filter (/= '+') >>> read) $ contents
      part1 = sum nums
      part2 = cycle >>> scanl (+) 0 >>> firstRepeat $ nums
  putStrLn $ "sum: " ++ show part1
  putStrLn $ "repeat: " ++ show part2

firstRepeat :: Ord a => [a] -> a
firstRepeat as = firstRepeat' as S.empty

firstRepeat' :: Ord a => [a] -> S.Set a -> a
firstRepeat' (a:as) aSet
  | S.member a aSet = a
  | otherwise = firstRepeat' as aSet'
  where
    aSet' = S.insert a aSet
