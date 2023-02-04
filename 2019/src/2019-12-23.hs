{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore: Use second -}
-- cabal run --enable-profiling day16 --  +RTS -p
import           Control.Arrow

import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Map        as M
import           Debug.Trace

import qualified IntCode         as IC

data State =
  State
    { machines :: M.Map Int IC.Machine
    , previous :: Int
    , message  :: Message
    , senty    :: Int
    , nat      :: Bool
    }
  deriving (Show)

type Message = (Int, Int, Int) -- address, X, Y

type Queue = [Message]

main :: IO ()
main = do
  contents <- readFile "data/2019-12-23.dat"
  let intcodes = IC.readIntcode contents
      (_, _, part1) = network intcodes 50 False
      (_, _, part2) = network intcodes 50 True
  putStrLn $ "Part One: " ++ show part1
  putStrLn $ "Part Two: " ++ show part2

network :: [Int] -> Int -> Bool -> Message
network intcodes size nat = runNetwork state queue
  where
    machines =
      map (\i -> (i, IC.runPrg intcodes [i, -1])) >>> M.fromList $
      [0 .. (size - 1)]
    queue = L.foldl' (\l m -> l ++ (toMessages m)) [] $ M.elems machines
    machines' = M.map IC.flushOutput machines
    state =
      State
        { machines = machines'
        , previous = 0
        , nat = nat
        , senty = (-1)
        , message = (999, 999, 999)
        }

runNetwork :: State -> Queue -> Message
runNetwork state []    = processNat state []
runNetwork state queue = processMessage state queue

processNat :: State -> Queue -> Message
processNat state []
  | not (nat state) = (0, x, y)
  | senty state == y = (0, x, y)
  | otherwise = runNetwork state' [(0, x, y)]
  where
    (_, x, y) = message state
    state' = state {senty = y}
processNat state (msg:queue) = runNetwork state' queue
  where
    state' = state {message = msg}

processMessage :: State -> Queue -> Message
processMessage state [] = (-1, -1, -1) -- we can't get here
processMessage state (msg:queue)
  | node == 255 && nat state = processNat state (msg : queue)
  | node == 255 = msg
  | otherwise = runNetwork state' (queue ++ queue')
  where
    ms = machines state
    (node, x, y) = msg
    mc = ms M.! node
    mc' = IC.runPrgAt $ IC.addSignal mc [x, y, -1]
    mc'' = IC.flushOutput mc'
    ms' = M.insert node mc'' ms
    state' = state {machines = ms', previous = node}
    queue' = toMessages mc'

toMessages :: IC.Machine -> [Message]
toMessages = IC.output >>> L.chunksOf 3 >>> map (\(a:b:c:_) -> (a, b, c))
