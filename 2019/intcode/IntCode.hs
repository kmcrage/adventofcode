module IntCode where

import qualified Data.Map as M

data State
  = Running
  | Paused
  | Halted
  deriving (Show, Eq)

--             Instructions, Input, Pointer, Offset, Output, State
data Machine =
  Machine
    { memory  :: M.Map Int Int
    , input   :: [Int]
    , pointer :: Int
    , offset  :: Int
    , output  :: [Int]
    , state   :: State
    }
  deriving (Show)

initMachine :: [Int] -> [Int] -> Machine
initMachine intcode signal =
  Machine
    { memory = M.fromList $ zip [0 ..] intcode
    , input = signal
    , pointer = 0
    , offset = 0
    , output = []
    , state = Running
    }

memory0 :: Machine -> Int
memory0 m = memory m M.! 0

memoryList :: Machine -> [Int]
memoryList m = map (\p -> M.findWithDefault 0 p mem) [0..pmax]
  where
    mem = memory m
    pmax = maximum $ M.keys mem

flushOutput :: Machine -> Machine
flushOutput m = m {output = []}

addSignal :: Machine -> [Int] -> Machine
addSignal m signal = m {input = input m ++ signal}

runPrg :: [Int] -> [Int] -> Machine
runPrg xs input = machine
  where
    init = initMachine xs input
    machine = runPrgAt init

runPrgAt :: Machine -> Machine
runPrgAt m
  | state mAfter /= Running = mAfter
  | otherwise = runPrgAt mAfter
  where
    xs = memory m
    p = pointer m
    opMode = xs M.! p
    op = mod opMode 100
    mode = div opMode 100
    opArgsRaw = map (\p -> M.findWithDefault 0 p xs) [(p + 1) .. (p + 3)]
    opArgs = processArgs op opArgsRaw mode m
    mAfter = runOp op opArgs m

processArgs :: Int -> [Int] -> Int -> Machine -> [Int]
processArgs 3 (idx:args) 2 m = (idx + offset m) : args
processArgs 3 raw _ _        = raw
processArgs _ raw mode m     = cookArgs raw mode m

cookArgs :: [Int] -> Int -> Machine -> [Int]
cookArgs [r] 2 m = [r + offset m]
cookArgs [r] _ _ = [r]
cookArgs (r:rs) mode m
  | mode' == 0 = memVal : cookArgs rs (div mode 10) m
  | mode' == 2 = offsetVal : cookArgs rs (div mode 10) m
  | otherwise = r : cookArgs rs (div mode 10) m
  where
    mode' = mod mode 10
    xs = memory m
    off = offset m
    memVal = M.findWithDefault 0 r xs
    offsetVal = M.findWithDefault 0 (off + r) xs

runOp :: Int -> [Int] -> Machine -> Machine
runOp 1 (a:b:rIdx:_) m =
  m {memory = runAdd a b rIdx (memory m), pointer = 4 + pointer m}
runOp 2 (a:b:rIdx:_) m =
  m {memory = runMult a b rIdx (memory m), pointer = 4 + pointer m}
runOp 3 (rIdx:_) m -- input
  | null (input m) = m {state = Paused}
  | otherwise =
    m
      { memory = runInput rIdx (memory m) (input m)
      , input = input'
      , pointer = pointer'
      , state = Running
      }
  where
    input' = tail $ input m
    pointer' = 2 + pointer m
runOp 4 (a:_) m = m {output = output m ++ [a], pointer = 2 + pointer m} -- output
runOp 5 (0:_) m = m {pointer = 3 + pointer m} -- jump if true
runOp 5 (_:q:_) m = m {pointer = q}
runOp 6 (0:q:_) m = m {pointer = q} -- jump if false
runOp 6 _ m = m {pointer = 3 + pointer m}
runOp 7 (a:b:rIdx:_) m =
  m {memory = runLessThan a b rIdx (memory m), pointer = 4 + pointer m}
runOp 8 (a:b:rIdx:_) m =
  m {memory = runEqual a b rIdx (memory m), pointer = 4 + pointer m}
runOp 9 (a:_) m = m {pointer = 2 + pointer m, offset = a + offset m}
runOp 99 _ m = m {pointer = 0, state = Halted}
runOp _ _ machine = machine

runAdd :: Int -> Int -> Int -> M.Map Int Int -> M.Map Int Int
runAdd a b rIdx xs = replaceElementAt xs rIdx (a + b)

runMult :: Int -> Int -> Int -> M.Map Int Int -> M.Map Int Int
runMult a b rIdx xs = replaceElementAt xs rIdx (a * b)

runLessThan :: Int -> Int -> Int -> M.Map Int Int -> M.Map Int Int
runLessThan a b rIdx xs
  | a < b = replaceElementAt xs rIdx 1
  | otherwise = replaceElementAt xs rIdx 0

runEqual :: Int -> Int -> Int -> M.Map Int Int -> M.Map Int Int
runEqual a b rIdx xs
  | a == b = replaceElementAt xs rIdx 1
  | otherwise = replaceElementAt xs rIdx 0

runInput :: Int -> M.Map Int Int -> [Int] -> M.Map Int Int
runInput rIdx xs input = replaceElementAt xs rIdx (head input)

replaceElementAt :: M.Map Int Int -> Int -> Int -> M.Map Int Int
replaceElementAt xs rIdx val = M.insert rIdx val xs
