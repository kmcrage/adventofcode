import Data.List.Split

main :: IO ()
main = do  
    -- contents <- readFile "data/2019-12-02.dat"
    contents <- readFile "data/test.dat"
    let intcodes = map read_int strcodes
        read_int n = read n :: Integer
        strcodes = splitOn "," contents
    print intcodes


