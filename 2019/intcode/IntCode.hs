module IntCode where
import Debug.Trace 

data State
  = Running
  | Paused
  | Halted
  deriving (Show, Eq)

--             Instructions, Input, Pointer, Offset, Output, State
type Machine = ([Int], [Int], Int, Int, [Int], State)

initMachine :: [Int] -> Int -> Machine
initMachine intcode signal = (intcode, [signal], 0, 0, [], Running)

outputM :: Machine -> [Int]
outputM (_, _, _, _, output, _) = output

pointerM :: Machine -> Int
pointerM (_, _, p, _, _, _) = p

offsetM :: Machine -> Int
offsetM (_, _, _, o, _, _) = o

memoryM :: Machine -> [Int]
memoryM (memory, _, _, _, _, _) = memory

memoryM0 :: Machine -> Int
memoryM0 = head . memoryM

stateM :: Machine -> State
stateM (_, _, _, _, _, state) = state

flushOutput :: Machine -> Machine
flushOutput (xs, input, p, o, output, state) = (xs, input, p, o, [], state)

addSignalM :: Machine -> [Int] -> Machine
addSignalM m signal = n
  where
    (intcodes, inputs, p, o, outputs, done) = m
    n = (intcodes, inputs ++ signal, p, o, outputs, done)

runPrg :: [Int] -> [Int] -> Machine
runPrg xs input = machine
  where
    init = (xs, input, 0, 0, [], Running)
    machine = runPrgAt init

runPrgAt :: Machine -> Machine
runPrgAt m
  | stateM mAfter /= Running = mAfter
  | otherwise = runPrgAt mAfter
  where
    xs = memoryM m
    p = pointerM m
    opMode = xs !! p
    op = mod opMode 100
    mode = div opMode 100
    opArgsRaw = drop (p + 1) $ take (p + 4) xs
    opArgs = processArgs op opArgsRaw mode m
    mAfter = runOp op opArgs m

processArgs :: Int -> [Int] -> Int -> Machine -> [Int]
processArgs 3 (idx:args) 2 m = (idx + offsetM m):args
processArgs 3 raw _ _ = raw
processArgs _ raw mode m = cookArgs raw mode m

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
  | mode == 2 = [a, b, c + offsetM m]
  | otherwise = [a,b,c]
maybeOffset mode m args = args


maybeReplaceElementAt :: Int -> Machine -> [Int] -> Int -> [Int]
maybeReplaceElementAt mode m raw idx
  | mode == 0 = replaceElementAt raw idx memVal
  | mode == 2 = replaceElementAt raw idx offsetVal
  | otherwise = raw
  where
    xs = memoryM m
    offset = offsetM m
    memVal = xs !! (raw !! idx)
    offsetVal = xs !!  (offset + (raw !! idx))

runOp :: Int -> [Int] -> Machine -> Machine
runOp 1 (a:b:rIdx:_) (xs, input, p, o, output, _) =
  (runAdd a b rIdx xs, input, p + 4, o, output, Running)
runOp 2 (a:b:rIdx:_) (xs, input, p, o, output, _) =
  (runMult a b rIdx xs, input, p + 4, o, output, Running)
runOp 3 (rIdx:_) (xs, input, p, o, output, _)
  | null input = (xs, input, p, o, output, Paused)
  | otherwise = (runInput rIdx xs input, tail input, p + 2, o, output, Running) -- input
runOp 4 (a:_) (xs, input, p, o, output, _) =
  (xs, input, p + 2, o, output ++ [a], Running) -- output
runOp 5 (0:_) (xs, input, p, o, output, _) =
  (xs, input, p + 3, o, output, Running) -- jump if true
runOp 5 (_:q:_) (xs, input, p, o, output, _) =
  (xs, input, q, o, output, Running)
runOp 6 (0:q:_) (xs, input, p, o, output, _) =
  (xs, input, q, o, output, Running) -- jump if false
runOp 6 _ (xs, input, p, o, output, _) = (xs, input, p + 3, o, output, Running)
runOp 7 (a:b:rIdx:_) (xs, input, p, o, output, _) =
  (runLessThan a b rIdx xs, input, p + 4, o, output, Running)
runOp 8 (a:b:rIdx:_) (xs, input, p, o, output, _) =
  (runEqual a b rIdx xs, input, p + 4, o, output, Running)
runOp 9 (a:_) (xs, input, p, o, output, state) =
  (xs, input, p + 2, o + a, output, state)
runOp 99 _ (xs, input, p, o, output, _) = (xs, input, 0, o, output, Halted)
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
