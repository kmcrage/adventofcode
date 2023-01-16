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
cookArgs raw mode m = cooked''
  where
    mode0 = mod mode 10
    mode1 = mod (div mode 10) 10
    mode2 = mod (div mode 100) 100
    cooked = maybeReplaceElementAt mode0 m raw 0
    cooked' = maybeReplaceElementAt mode1 m cooked 1
    cooked'' = maybeOffset mode2 m cooked' -- this is a write index

maybeOffset :: Int -> Machine -> [Int] -> [Int]
maybeOffset mode m (a:b:c:_)
  | mode == 2 = [a, b, c + offset m]
  | otherwise = [a, b, c]
maybeOffset mode m args = args

maybeReplaceElementAt :: Int -> Machine -> [Int] -> Int -> [Int]
maybeReplaceElementAt mode m raw idx
  | mode == 0 = replaceElementAt raw idx memVal
  | mode == 2 = replaceElementAt raw idx offsetVal
  | otherwise = raw
  where
    xs = memory m
    off = offset m
    memVal = xs !! (raw !! idx)
    offsetVal = xs !! (off + (raw !! idx))

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
replaceElementAt xs rIdx val = pre ++ [val] ++ post
  where
    pre = take rIdx xs
    post = drop (rIdx + 1) xs
