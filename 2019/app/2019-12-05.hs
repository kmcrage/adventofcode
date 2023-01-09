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
    putStrLn $ "Diagnostics One: " ++ show diagnostics_one
    putStrLn $ "Result One: " ++ show result_one

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
        opLen = oplen op
        opArgsRaw = drop (p + 1) $ take (p + opLen) xs
        opArgs = processArgs op opArgsRaw mode xs
        (xsAfter, oAfter) = runOp op opArgs xs input
        mAfter = (xsAfter, input, p + opLen, output ++ oAfter)

oplen :: Int -> Int
oplen 3 = 2
oplen 4 = 2
oplen _ = 4

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
maybeReplaceElementAt 1 xs raw idx = replaceElementAt raw idx (raw !! idx)


runOp :: Int -> [Int] -> [Int] -> [Int] -> ([Int], [Int])
runOp 1 [a, b, rIdx] xs _ = (runAdd a b rIdx xs, [])
runOp 2 [a, b, rIdx] xs _ = (runMult a b rIdx xs, [])
runOp 3 (rIdx:_) xs input = (runInput rIdx xs input, [])
runOp 4 (a:_) xs _ = (xs, [a])
runOp _ _ xs _ = (xs, [])

runAdd :: Int -> Int -> Int -> [Int] -> [Int]
runAdd a b rIdx xs = replaceElementAt xs rIdx (a + b)

runMult :: Int -> Int -> Int -> [Int] -> [Int]
runMult a b rIdx xs = replaceElementAt xs rIdx (a * b)

runInput :: Int -> [Int] -> [Int] -> [Int]
runInput rIdx xs input = replaceElementAt xs rIdx (head input)

replaceElementAt :: [Int] -> Int -> Int -> [Int]
replaceElementAt xs rIdx val =
    pre ++ [val] ++ post
    where
        pre = take rIdx xs
        post = drop (rIdx + 1) xs

