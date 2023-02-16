{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow   ((&&&), (>>>))
import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Debug.Trace     (trace)

type Growth = M.Map String Char

type State = (String, Int)

main :: IO ()
main = do
  contents <- readFile "data/2018-12-12.dat"
  -- contents <- readFile "data/test.dat"
  let (growth, state) = parse contents
      is = iterate (grow growth) (state, 0)
      score20 = score . head . drop 20 $ is
      pt2 = scoreLong is 50000000000
  putStrLn $ "20 generations: " ++ show score20
  putStrLn $ "part 2, many iterations: " ++ show pt2

scoreLong :: [State] -> Int -> Int
scoreLong ss n = score (state, offset + cycles * dOffset)
  where
    (start, period, dOffset) = findRepeat ss
    start' = start + (mod n period)
    cycles = div (n - start') period
    (state, offset) = head . drop start' $ ss

findRepeat :: [State] -> (Int, Int, Int)
findRepeat is = (start, period, dOffset)
  where
    is' =
      drop 1 .
      L.scanl'
        (\(i, m, _) (s, o) -> (i + 1, M.insertWith (min) s (i + 1) m, (s, o)))
        (-1, M.empty, ("", 0)) $
      is
    periodic = dropWhile (\(i, m, (s, _)) -> i == m M.! s) $ is'
    (i, m, (s, o)) = head periodic
    start = m M.! s
    period = i - start
    (_, _, (_, o')) = head . drop period $ periodic
    dOffset = o' - o

score :: State -> Int
score (state, offset) =
  zip [offset ..] >>> filter ((== '#') . snd) >>> map fst >>> sum $ state

grow :: Growth -> State -> State
grow growth (state, offset) = (state', offset')
  where
    stateE = "...." ++ state ++ "...." -- this creates an offset of -2
    stateE' = L.divvy 5 1 >>> map (update growth) $ stateE
    stateT = dropWhile (== '.') stateE'
    state' = reverse >>> dropWhile (== '.') >>> reverse $ stateT
    offset' = offset - 2 + (length stateE') - (length stateT)

update :: Growth -> String -> Char
update growth s
  | M.member s growth = growth M.! s
  | otherwise = '.'

parse :: String -> (Growth, String)
parse input = (growth, state)
  where
    ls = lines input
    tokens = words . head $ ls
    state = tokens !! 2
    growth =
      drop 2 >>>
      takeWhile (/= "") >>>
      map (L.splitOn " => ") >>> map (\(p:c:_) -> (p, head c)) >>> M.fromList $
      ls
