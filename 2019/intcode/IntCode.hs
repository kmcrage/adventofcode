module IntCode where

import           Debug.Trace

data State
  = Running
  | Paused
  | Halted
  deriving (Show, Eq)

--             Instructions, Input, Pointer, Offset, Output, State
data Machine =
  Machine
    { memory  :: [Int]
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
    { memory = intcode
    , input = signal
    , pointer = 0
    , offset = 0
    , output = []
    , state = Running
    }

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
    opMode = xs !! p
    op = mod opMode 100
    mode = div opMode 100
    opArgsRaw = drop (p + 1) $ take (p + 4) xs
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
    overlen = max 0 (1 + r - length xs + max 0 off)
    xs'
      | overlen > 0 = xs ++ replicate overlen 0
      | otherwise = xs
    memVal = xs' !! r
    offsetVal = xs' !! (off + r)

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

runAdd :: Int -> Int -> Int -> [Int] -> [Int]
runAdd a b rIdx xs = replaceElementAt xs rIdx (a + b)

runMult :: Int -> Int -> Int -> [Int] -> [Int]
runMult a b rIdx xs = replaceElementAt xs rIdx (a * b)

runLessThan :: Int -> Int -> Int -> [Int] -> [Int]
runLessThan a b rIdx xs
  | a < b = replaceElementAt xs rIdx 1
  | otherwise = replaceElementAt xs rIdx 0

runEqual :: Int -> Int -> Int -> [Int] -> [Int]
runEqual a b rIdx xs
  | a == b = replaceElementAt xs rIdx 1
  | otherwise = replaceElementAt xs rIdx 0

runInput :: Int -> [Int] -> [Int] -> [Int]
runInput rIdx xs input = replaceElementAt xs rIdx (head input)

replaceElementAt :: [Int] -> Int -> Int -> [Int]
replaceElementAt xs rIdx val = pre ++ val : post
  where
    xs' = xs ++ replicate (max 0 (1 + rIdx - length xs)) 0
    pre = take rIdx xs'
    post = drop (rIdx + 1) xs
