{- HLINT ignore: Use second -}
import           Control.Arrow
import qualified Data.List       as L
import           Data.List.Extra as E
import qualified Data.Map        as M
import qualified Data.Set        as S

import           Debug.Trace

type Coord = (Int, Int, Int)

type Bugs = S.Set Coord

main :: IO ()
main
  -- contents <- readFile "data/2019-12-24.dat"
 = do
  contents <- readFile "data/test.dat"
  let bugs = parse contents
      part2 = iterate breed >>> drop 200 >>> head $ bugs
  putStrLn $ "Num bugs: " ++ show (length part2)

breed :: Bugs -> Bugs
breed bugs =
  filter (isBug bugs) >>> S.fromList $
  [ (i, j, k)
  | i <- [0 .. 4]
  , j <- [0 .. 4]
  , k <- [(minK - 1) .. (maxK + 1)]
  , i /= 2 || j /= 2
  ]
  where
    ks = S.toList >>> map (\(_, _, k) -> k) $ bugs
    minK = minimum ks
    maxK = maximum ks

isBug :: Bugs -> Coord -> Bool
isBug bugs c
  | nghrs == 1 = True
  | S.notMember c bugs && nghrs == 2 = True
  | otherwise = False
  where
    invertI (i, j, k) = (4 - i, j, k)
    flip (i, j, k) = (j, i, k)
    nhbrS = invertI >>> nhbrN >>> map invertI
    nhbrE = flip >>> nhbrS >>> map flip
    nhbrW = flip >>> nhbrN >>> map flip
    nghrs =
      filter (`S.member` bugs) >>> length $
      nhbrN c ++ nhbrS c ++ nhbrE c ++ nhbrW c

nhbrN :: Coord -> [Coord]
nhbrN (i, j, k)
  | i == 0 = [(1, 2, k + 1)]
  | i == 3 && j == 2 = [(4, a, k - 1) | a <- [0 .. 4]]
  | otherwise = [(i - 1, j, k)]

view :: Bugs -> String
view bugs = map (viewL bugs) >>> intercalate "\n" $ [minK .. maxK]
  where
    ks = S.toList >>> map (\(_, _, k) -> k) $ bugs
    minK = minimum ks
    maxK = maximum ks

viewL :: Bugs -> Int -> String
viewL bugs k = map toChr [(i, j, k) | i <- [0 .. 4], j <- [0 .. 5]]
  where
    toChr (i, j, k)
      | S.member (i, j, k) bugs = '#'
      | j == 5 = '\n'
      | otherwise = '.'

parse :: String -> Bugs
parse =
  lines >>>
  zip [0 ..] >>>
  concatMap coordChar >>>
  filter (\(i, j, c) -> c == '#') >>>
  map (\(i, j, c) -> (j, i, 0)) >>> S.fromList

coordChar :: (Int, String) -> [(Int, Int, Char)]
coordChar (j, line) = zipWith (\i char -> (i, j, char)) [0 ..] line
