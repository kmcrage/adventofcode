import           Control.Arrow
import           Data.List.Split
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Debug.Trace

import           IntCode

type Coord = (Int, Int)

data Robot =
  Robot
    { machine  :: Machine
    , position :: Coord
    , distance :: Int
    }
  deriving (Show)

main :: IO ()
main = do
  contents <- readFile "data/2019-12-15.dat"
  -- contents <- readFile "data/test.dat"
  let intcodes = readIntcode contents
      m = initMachine intcodes []
      m' = m {output = [1]}
      visited = S.empty :: S.Set Coord
      final =
        bfs
          foundOxy
          visited
          [Robot {machine = m', position = (0, 0), distance = 0}]
      oxyPos = position final
      oxyMachine = machine final -- need a machine initialised to the oxy cylinder position
      furthest =
        bfs
          (const False) -- halt when we run out of candidates
          visited
          [Robot {machine = oxyMachine, position = oxyPos, distance = 0}]
  putStrLn $ "Distance to Oxygen: " ++ show (distance final)
  putStrLn $ "Distance from Oxygen: " ++ show (distance furthest)

foundOxy :: Robot -> Bool
foundOxy robot = output (machine robot) == [2]

bfs :: (Robot -> Bool) -> S.Set Coord -> [Robot] -> Robot
bfs halt visited (robot:queue)
  | halt robot = robot -- this is for part one
  -- | S.member pos visited = bfs halt visited queue -- this could confuse part two, prefilter instead
  | null queue' = robot -- this is the case where we search exhaustively i.e. part two
  | otherwise = bfs halt visited' queue'
  where
    pos = position robot
    m = machine robot
    m' = flushOutput m
    visited' = S.insert pos visited
    robot' = robot {machine = m'}
    queue' =
      filter (\r -> S.notMember (position r) visited') $
      queue ++ neighbours robot' -- not the most efficient, but handy for part two

neighbours :: Robot -> [Robot]
neighbours robot = nhbrs
  where
    (x, y) = position robot
    m = machine robot
    d = distance robot
    nhbrs =
      map
        (\(s, pos) ->
           robot {position = pos, machine = m {input = s}, distance = d + 1}) >>>
      map (\r -> r {machine = runPrgAt (machine r)}) >>>
      filter (\r -> output (machine r) /= [0]) $
      [ ([1], (x, y + 1))
      , ([2], (x, y - 1))
      , ([3], (x + 1, y))
      , ([4], (x - 1, y))
      ]
