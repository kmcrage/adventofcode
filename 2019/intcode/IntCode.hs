module IntCode where

data State
  = Running
  | Paused
  | Halted
  deriving (Show, Eq)

--             Instructions, Input, Pointer, Output, State
type Machine = ([Int], [Int], Int, [Int], State)

outputM :: Machine -> [Int]
outputM (_, _, _, output, _) = output

memoryM :: Machine -> [Int]
memoryM (memory, _, _, _, _) = memory

memoryM0 :: Machine -> Int
memoryM0 = head . memoryM

stateM :: Machine -> State
stateM (_, _, _, _, state) = state

runPrg :: [Int] -> [Int] -> Machine
runPrg xs input = machine
  where
    init = (xs, input, 0, [], Running)
    machine = runPrgAt init

runPrgAt :: Machine -> Machine
runPrgAt (xs, input, p, output, state)
  | p > length xs = mAfter
  | stateM mAfter /= Running = mAfter
  | otherwise = runPrgAt mAfter
  where
    opMode = xs !! p
    op = mod opMode 100
    mode = div opMode 100
    opArgsRaw = drop (p + 1) $ take (p + 4) xs
    opArgs = processArgs op opArgsRaw mode xs
    mAfter = runOp op opArgs (xs, input, p, output, state)

processArgs :: Int -> [Int] -> Int -> [Int] -> [Int]
processArgs 3 raw _ _     = raw
processArgs _ raw mode xs = cookArgs raw mode xs

cookArgs :: [Int] -> Int -> [Int] -> [Int]
cookArgs raw mode xs = cooked'
  where
    mode0 = mod mode 10
    mode1 = mod (div mode 10) 10
    mode2 = mod (div mode 100) 10
    cooked = maybeReplaceElementAt mode0 xs raw 0
    cooked' = maybeReplaceElementAt mode1 xs cooked 1

maybeReplaceElementAt :: Int -> [Int] -> [Int] -> Int -> [Int]
maybeReplaceElementAt 0 xs raw idx =
  replaceElementAt raw idx (xs !! (raw !! idx))
maybeReplaceElementAt 1 _ raw _ = raw

runOp :: Int -> [Int] -> Machine -> Machine
runOp 1 (a:b:rIdx:_) (xs, input, p, output, _) =
  (runAdd a b rIdx xs, input, p + 4, output, Running)
runOp 2 (a:b:rIdx:_) (xs, input, p, output, _) =
  (runMult a b rIdx xs, input, p + 4, output, Running)
runOp 3 (rIdx:_) (xs, input, p, output, _)
  | null input = (xs, input, p, output, Paused)
  | otherwise = (runInput rIdx xs input, tail input, p + 2, output, Running) -- input
runOp 4 (a:_) (xs, input, p, output, _) =
  (xs, input, p + 2, output ++ [a], Running) -- output
runOp 5 (0:_) (xs, input, p, output, _) = (xs, input, p + 3, output, Running) -- jump if true
runOp 5 (_:q:_) (xs, input, p, output, _) = (xs, input, q, output, Running)
runOp 6 (0:q:_) (xs, input, p, output, _) = (xs, input, q, output, Running) -- jump if false
runOp 6 _ (xs, input, p, output, _) = (xs, input, p + 3, output, Running)
runOp 7 (a:b:rIdx:_) (xs, input, p, output, _) =
  (runLessThan a b rIdx xs, input, p + 4, output, Running)
runOp 8 (a:b:rIdx:_) (xs, input, p, output, _) =
  (runEqual a b rIdx xs, input, p + 4, output, Running)
runOp 99 _ (xs, input, p, output, _) = (xs, input, 0, output, Halted)
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
