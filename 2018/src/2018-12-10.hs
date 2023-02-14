{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow
import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Debug.Trace

type Coord = (Int, Int)

type Point = (Coord, Coord)

main :: IO ()
main
 = do
  contents <- readFile "data/2018-12-10.dat"
  -- contents <- readFile "data/test.dat"
  let pts = parse contents
      (time, message) = getMin pts
  -- head . dropWhile ((> 10) . height . snd) . zip [0 ..] . iterate update $ pts
  -- simpler but hard-codes the size
  putStrLn $ "Time: " ++ show time ++ "\nMessage:\n\n" ++ view message

-- fins the item where the area of the pts is minimal
getMin :: [Point] -> (Int, [Point])
getMin pts = result
  where
    stream = iterate update >>> map (id &&& area) $ pts
    -- duplicate the stream offset by one so you can compare areas
    result =
      drop 1 >>>
      zip3 [0 ..] stream >>>
      dropWhile (\(_, (_, hp), (_, hq)) -> hp > hq) >>> -- lowest area
      map (\(i, (p, _), _) -> (i, p)) >>> head $
      stream

area :: [Point] -> Int
area pts = h * w
  where
    h = map (snd . fst) >>> minimum &&& maximum >>> uncurry subtract $ pts
    w = map (fst . fst) >>> minimum &&& maximum >>> uncurry subtract $ pts

update :: [Point] -> [Point]
update = map (\((i, j), (u, v)) -> ((i + u, j + v), (u, v)))

view :: [Point] -> String
view pts = map toChr [(i, j) | j <- [jmn .. jmx], i <- [imn .. (imx + 1)]]
  where
    ps = map fst pts
    psSet = S.fromList ps
    (imn, imx) = (minimum &&& maximum) . map fst $ ps
    (jmn, jmx) = (minimum &&& maximum) . map snd $ ps
    toChr (i, j)
      | i == imx + 1 = '\n'
      | S.member (i, j) psSet = '#'
      | otherwise = '.'

parse :: String -> [Point]
parse = lines >>> map parseLine

parseLine :: String -> Point
parseLine line = ((a, b), (c, d))
  where
    tokens = splitOnAnyOf ["<", ">", ","] line
    (a:b:c:d:_) = map (tokens L.!!) >>> map (read @Int) $ [1, 2, 4, 5]

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = L.foldl' (\ys d -> ys >>= L.splitOn d) [xs] ds
