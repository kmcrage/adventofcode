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
      iRobot = Robot {machine = m', position = (0, 0), distance = 0}
      oxyRobot = bfs iRobot foundOxy visited [iRobot]
      oxyPos = position oxyRobot
      oxyMachine = machine oxyRobot -- need a machine initialised to the oxy cylinder position
      oRobot = Robot {machine = oxyMachine, position = oxyPos, distance = 0}
      furthest = bfs oRobot (const False) visited [oRobot]
  putStrLn $ "Distance to Oxygen: " ++ show (distance oxyRobot)
  putStrLn $ "Distance from Oxygen: " ++ show (distance furthest)

foundOxy :: Robot -> Bool
foundOxy robot = output (machine robot) == [2]

bfs :: Robot -> (Robot -> Bool) -> S.Set Coord -> [Robot] -> Robot
bfs prev halt visited (robot:queue)
  | halt robot = robot -- this is for part one
  | S.member pos visited && null queue = prev
  | S.member pos visited = bfs prev halt visited queue
  | null queue' = robot -- this is the case where we search exhaustively i.e. part two
  | otherwise = bfs robot halt visited' queue'
  where
    pos = position robot
    m = machine robot
    m' = flushOutput m
    visited' = S.insert pos visited
    robot' = robot {machine = m'}
    queue' = queue ++ neighbours robot'

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
