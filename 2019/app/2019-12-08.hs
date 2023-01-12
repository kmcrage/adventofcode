import           Data.List
import           Data.List.Split

main :: IO ()
main = do
  contents <- readFile "data/2019-12-08.dat"
    -- contents <- readFile "data/test.dat"
  let width = 25
      height = 6
      size = width * height
      line = head (lines contents)
      layers = chunksOf size line
      layer = head $ sortOn (countLetters '0') layers
      result = countLetters '1' layer * countLetters '2' layer
      final = map image $ foldl' combine (replicate size '2') layers
      table = chunksOf width final
  putStrLn $ "part one: " ++ show result
  putStrLn "part two: \n"
  putStrLn $ unlines table

countLetters :: Char -> String -> Int
countLetters c = length . filter (== c)

combine :: String -> String -> String
combine a b = map transparency zlist
  where
    zlist = zip a b

transparency :: (Char, Char) -> Char
transparency (s, t)
  | s == '2' = t
  | otherwise = s

image :: Char -> Char
image s
  | s == '0' = ' '
  | otherwise = 'X'
