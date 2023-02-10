import           Control.Arrow
import qualified Data.Char     as C
import qualified Data.List     as L

main :: IO ()
main = do
  contents <- readFile "data/2018-12-05.dat"
  -- contents <- readFile "data/test.dat"
  let reagents = lines >>> head $ contents
      part1 = L.foldr react "" >>> length $ reagents
      part2 = map (reactWithout reagents) >>> minimum $ [65 .. 90] -- upper case
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2

reactWithout :: String -> Int -> Int
reactWithout reagents o = L.foldr react "" >>> length $ reagents'
  where
    reagents' =
      filter
        (\l ->
           let c = C.chr o
            in l /= c && l /= C.toLower c)
        reagents

react :: Char -> String -> String
react c [] = [c]
react c string
  | s >= 'a' && C.toUpper s == c = str
  | s <= 'Z' && C.toLower s == c = str
  | otherwise = c : string
  where
    (s:str) = string
