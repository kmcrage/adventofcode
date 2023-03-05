{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow ((&&&), (>>>))
import qualified Data.Char     as C
import qualified Data.List     as L
import qualified Data.Map      as M
import           Debug.Trace   (trace)

type Coord = (Int, Int)

type Land = M.Map Coord Cell

data Cell
  = Lumberyard
  | Trees
  | Open
  deriving (Enum, Eq, Show, Ord)

main :: IO ()
main = do
  contents <- readFile "data/2018-12-18.dat"
  -- contents <- readFile "data/test.dat"
  let land = parse contents
      sz = squareRoot $ length land
      lands = L.iterate (automata (sz - 1)) land
      pt1 = score . head . drop 10 $ lands
      (fst, rpt) = findRepeat lands
      offset = mod (1000000000 - fst) rpt
      pt2 = score . head . drop (fst + offset) $ lands
  putStrLn $ "pt1: " ++ show pt1
  putStrLn $ "pt2: " ++ show pt2

score :: Land -> Int
score land = lumberyards * trees
  where
    lumberyards = length $ M.filter (== Lumberyard) land
    trees = length $ M.filter (== Trees) land

findRepeat :: [Land] -> (Int, Int)
findRepeat lands = (first, rpt)
  where
    start = head lands
    init = (M.singleton start 0, start, 0)
    (map, rptLand, second) =
      zip [0 ..] >>>
      L.scanl' (\(m, _, _) (n, l) -> (M.insertWith min l n m, l, n)) init >>>
      dropWhile (\(m, l, n) -> m M.! l == n) >>> head $
      lands
    first = map M.! rptLand
    rpt = second - first

automata :: Int -> Land -> Land
automata mx land =
  map (\c -> (c, update land c)) >>> M.fromList >>> M.filter (/= Open) $
  [(i, j) | i <- [0 .. mx], j <- [0 .. mx]]

update :: Land -> Coord -> Cell
update land coord
  | cell == Open && trees >= 3 = Trees
  | cell == Trees && lumberyards >= 3 = Lumberyard
  | cell == Lumberyard && lumberyards >= 1 && trees >= 1 = Lumberyard
  | cell == Lumberyard = Open
  | otherwise = cell
  where
    cell = M.findWithDefault Open coord land
    --
    nhbrs =
      filter (\c -> c /= (0, 0)) >>>
      map (addC coord) >>> map (\c -> M.findWithDefault Open c land) $
      [(i, j) | i <- [-1 .. 1], j <- [-1 .. 1]]
    addC (a, b) (c, d) = (a + c, b + d)
    --
    trees = length . filter (== Trees) $ nhbrs
    lumberyards = length . filter (== Lumberyard) $ nhbrs

parse :: String -> Land
parse input =
  lines >>>
  zip [0 ..] >>>
  concatMap coordChar >>> map (\(i, j, c) -> ((i, j), toCell c)) >>> M.fromList $
  input
  where
    coordChar (j, line) = zipWith (\i char -> (j, i, char)) [0 ..] line
    toCell c
      | c == '|' = Trees
      | c == '#' = Lumberyard
      | otherwise = Open

squareRoot :: Integral t => t -> t
squareRoot n
  | n > 0 = babylon n
  | n == 0 = 0
  | n < 0 = error "Negative input"
  where
    babylon a
      | a > b = babylon b
      | True = a
      where
        b = quot (a + quot n a) 2
