{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow   ((&&&), (>>>))
import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Map        as M
import           Data.MemoTrie   (memo2)
import qualified Data.Set        as S
import           Debug.Trace     (trace)

type Coord = (Int, Int)

main :: IO ()
main = do
  let c = fst $ maxCoord 300 8444 3
      pt2 = globalMaxCoord 300 8444 -- s=18 90,269,16
  putStrLn $ "Coord: " ++ show c
  putStrLn $ "Coord, Size: " ++ show pt2

globalMaxCoord :: Int -> Int -> (Int, Int, Int)
globalMaxCoord grid serial = (i, j, size)
  where
    maxs =
      map (id &&& maxCoord grid serial) >>> takeWhile (\(s, (c, m)) -> m > 0) $
      [1 .. (grid - 1)]
    gmax = maximum . map (\(_, (_, m)) -> m) $ maxs
    (size, ((i, j), _)) = head . filter (\(_, (_, m)) -> m >= gmax) $ maxs

maxCoord :: Int -> Int -> Int -> (Coord, Int)
maxCoord grid s r = M.findMin . M.filter (>= maxPwrSum) $ pwrSum
  where
    pSum s r (i, j) =
      powerSAT s (i - 1, j - 1) + powerSAT s (i + r - 1, j + r - 1) -
      powerSAT s (i + r - 1, j - 1) -
      powerSAT s (i - 1, j + r - 1)
    pwrSum =
      M.fromList . map (id &&& pSum s r) $
      [(x, y) | x <- [1 .. (grid - r)], y <- [1 .. (grid - r)]]
    maxPwrSum = maximum . M.elems $ pwrSum

powerSAT :: Int -> Coord -> Int
powerSAT = memo2 powerSAT'
  where
    powerSAT' s (0, _) = 0
    powerSAT' s (_, 0) = 0
    powerSAT' s (x, y) =
      power s (x, y) + powerSAT s (x, y - 1) + powerSAT s (x - 1, y) -
      powerSAT s (x - 1, y - 1)

power :: Int -> Coord -> Int
power s (x, y) = p' - 5
  where
    rackid = x + 10
    p = (rackid * y + s) * rackid
    p' = mod (div p 100) 10
