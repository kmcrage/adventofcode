{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow
import qualified Data.List       as L
import qualified Data.List.Split as L
import           Debug.Trace

data Node =
  Node
    { metadata :: [Int]
    , children :: [Node]
    }
  deriving (Show)

main :: IO ()
main = do
  contents <- readFile "data/2018-12-08.dat"
  -- contents <- readFile "data/test.dat"
  let node = parse contents
      part1 = nodeSum node
      part2 = nodeValue node
  putStrLn $ "Sum: " ++ show part1
  putStrLn $ "Value: " ++ show part2

nodeValue :: Node -> Int
nodeValue node
  | null kids = sum idx
  | otherwise = value
  where
    idx = metadata node
    kids = children node
    value =
      map (subtract 1) >>>
      filter (< length kids) >>> map (kids !!) >>> map nodeValue >>> sum $
      idx

nodeSum :: Node -> Int
nodeSum node = kidsSum + metaSum
  where
    kidsSum = children >>> map nodeSum >>> sum $ node
    metaSum = sum $ metadata node

parse :: String -> Node
parse = L.splitOn " " >>> map (read @Int) >>> getNodes [] 1 >>> fst >>> head

getNodes :: [Node] -> Int -> [Int] -> ([Node], [Int])
getNodes nodes num input
  | num == 0 = (nodes, input)
  | otherwise = getNodes nodes' (num - 1) input'
  where
    ((n:md:_), inputNoHeader) = L.splitAt 2 input
    (kids, inputNoKids) = getNodes [] n inputNoHeader
    (meta, input') = L.splitAt md inputNoKids
    node = Node {metadata = meta, children = kids}
    nodes' = nodes ++ [node]
