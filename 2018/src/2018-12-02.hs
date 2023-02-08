import           Control.Arrow
import qualified Data.List     as L
import qualified Data.Set      as S
import           Debug.Trace

main :: IO ()
main = do
  contents <- readFile "data/2018-12-02.dat"
  -- contents <- readFile "data/test.dat"
  let ls = lines contents
      lens n =
        map (L.group . L.sort) >>>
        map (filter ((== n) . length)) >>> filter (not . null) >>> length $
        ls
      part1 = lens 3 * lens 2
      part2 = nearest ls "" []
  putStrLn $ "Part 1, product: " ++ show part1
  putStrLn $ "Part 2, match: " ++ show part2



nearest :: [String] -> String -> [String] -> String
nearest [] _ [] = "fail"
nearest (a:as) _ [] = nearest as a as
nearest ws a (b:bs) 
  | near a b = result
  | otherwise = nearest ws a bs
  where 
    result = filter (uncurry (==)) >>> map fst $ zip a b 

near :: String -> String -> Bool 
near a b 
  | diff==1 = True
  | otherwise = False 
  where 
    diff = filter (uncurry (/=)) >>> length $ zip a b
