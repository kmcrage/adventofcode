import Data.List.Split

main :: IO ()
main = do  
    contents <- readFile "data/2019-12-02.dat"
    -- contents <- readFile "data/test.dat"
    let intcodes = map read_int strcodes
        read_int n = read n :: Int
        strcodes = splitOn "," contents
        intcodes' = replaceElementAt intcodes 1 12
        intcodes'' = replaceElementAt intcodes' 2 2
        result = runPrg intcodes''
        solution = findNounVerb intcodes 19690720
        nounVerb = head solution * 100 + (solution !! 1)
    putStrLn $ "Output: " ++ show result 
    putStrLn $ "Noun, verb: " ++ show nounVerb

findNounVerb :: [Int] -> Int -> [Int]
findNounVerb xs ans = findNounVerbIter xs ans 0 0 

findNounVerbIter :: [Int] -> Int -> Int -> Int -> [Int]
findNounVerbIter xs ans noun verb 
    | runPrg xsnv == ans     = [noun, verb]
    | noun < 99             = findNounVerbIter xs ans (noun + 1) verb 
    | otherwise             = findNounVerbIter xs ans 0 (verb + 1)  
    where
        xsn = replaceElementAt xs 1 noun
        xsnv = replaceElementAt xsn 2 verb

runPrg :: [Int] -> Int
runPrg xs = head memory
    where
        memory = runPrgAt xs 0

runPrgAt :: [Int] -> Int -> [Int]
runPrgAt xs p
    | p > length xs = xsAfter
    | op == 99      = xsAfter
    | otherwise     = runPrgAt xsAfter (p + 4)
    where
        op = xs !! p
        xsAfter = runOp opArgs xs
        opArgs = drop p $ take (p + 4) xs

runOp :: [Int] -> [Int] -> [Int]
runOp [1, aIdx, bIdx, rIdx] xs = runAdd (xs !! aIdx) (xs !! bIdx) rIdx xs
runOp [2, aIdx, bIdx, rIdx] xs = runMult (xs !! aIdx) (xs !! bIdx) rIdx xs
runOp (99:_) xs = xs

runAdd :: Int -> Int -> Int -> [Int] -> [Int]
runAdd a b rIdx xs = replaceElementAt xs rIdx (a + b)

runMult :: Int -> Int -> Int -> [Int] -> [Int]
runMult a b rIdx xs = replaceElementAt xs rIdx (a * b)

replaceElementAt :: [Int] -> Int -> Int -> [Int]
replaceElementAt xs rIdx val =
    pre ++ [val] ++ post
    where
        pre = take rIdx xs
        post = drop (rIdx + 1) xs

