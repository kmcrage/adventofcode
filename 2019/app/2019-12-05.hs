import Debug.Trace
import Data.List.Split

main :: IO ()
main = do  
    contents <- readFile "data/2019-12-05.dat"
    -- contents <- readFile "data/test.dat"
    let intcodes = map read_int strcodes
        read_int n = read n :: Int
        strcodes = splitOn "," contents

        part_one = runPrg intcodes [1] 
        (_, _, _, diagnostics_one) = part_one
        result_one = last diagnostics_one

        part_two = runPrg intcodes [5] 
        (_, _, _, diagnostics_two) = part_two
        result_two = last diagnostics_two

    putStrLn $ "Diagnostics One: " ++ show diagnostics_one
    putStrLn $ "Result One: " ++ show result_one
    putStrLn $ "Diagnostics Two: " ++ show diagnostics_two

--             Instructions, Input, Pointer, Output
type Machine = ([Int], [Int], Int, [Int])

runPrg :: [Int] -> [Int] -> Machine
runPrg xs input = machine
    where
        init = (xs, input, 0, [])
        machine = runPrgAt init

runPrgAt :: Machine -> Machine
runPrgAt (xs, input, p, output)
    | p > length xs = mAfter
    | op == 99      = mAfter
    | otherwise     = runPrgAt mAfter
    where
        opMode = xs !! p
        op = mod opMode 100
        mode = div opMode 100
        opArgsRaw = drop (p + 1) $ take (p + 4) xs
        opArgs = processArgs op opArgsRaw mode xs
        mAfter = runOp op opArgs (xs, input, p, output)

processArgs :: Int -> [Int] -> Int -> [Int] -> [Int]
processArgs 3 raw _ _ = raw
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
maybeReplaceElementAt 0 xs raw idx = replaceElementAt raw idx (xs !! (raw !! idx))
maybeReplaceElementAt 1 _ raw _ = raw


runOp :: Int -> [Int] -> Machine -> Machine
runOp 1 (a:b:rIdx:_) (xs, input, p, output) = (runAdd a b rIdx xs, input, p + 4, output)
runOp 2 (a:b:rIdx:_) (xs, input, p, output) = (runMult a b rIdx xs, input, p + 4, output)
runOp 3 (rIdx:_) (xs, input, p, output) = (runInput rIdx xs input, input, p + 2, output)
runOp 4 (a:_) (xs, input, p, output) = (xs, input, p + 2, output ++ [a]) -- output
runOp 5 (0:_) (xs, input, p, output) = (xs, input, p + 3, output) -- jump if true
runOp 5 (_:q:_) (xs, input, p, output) = (xs, input, q, output)
runOp 6 (0:q:_) (xs, input, p, output) = (xs, input, q, output) -- jump if false
runOp 6 _ (xs, input, p, output) = (xs, input, p + 3, output)
runOp 7 (a:b:rIdx:_) (xs, input, p, output) = (runLessThan a b rIdx xs, input, p + 4, output)
runOp 8 (a:b:rIdx:_) (xs, input, p, output) = (runEqual a b rIdx xs, input, p + 4, output)
runOp _ _ machine = machine

runAdd :: Int -> Int -> Int -> [Int] -> [Int]
runAdd a b rIdx xs = replaceElementAt xs rIdx (a + b)

runMult :: Int -> Int -> Int -> [Int] -> [Int]
runMult a b rIdx xs = replaceElementAt xs rIdx (a * b)

runLessThan :: Int -> Int -> Int -> [Int] -> [Int]
runLessThan a b rIdx xs
    | a<b       = replaceElementAt xs rIdx 1
    | otherwise = replaceElementAt xs rIdx 0

runEqual :: Int -> Int -> Int -> [Int] -> [Int]
runEqual a b rIdx xs
    | a==b      = replaceElementAt xs rIdx 1
    | otherwise = replaceElementAt xs rIdx 0

runInput :: Int -> [Int] -> [Int] -> [Int]
runInput rIdx xs input = replaceElementAt xs rIdx (head input)



replaceElementAt :: [Int] -> Int -> Int -> [Int]
replaceElementAt xs rIdx val =
    pre ++ [val] ++ post
    where
        pre = take rIdx xs
        post = drop (rIdx + 1) xs
