
main :: IO ()
main = do  
    contents <- readFile "data/2019-12-01.dat"
    -- contents <- readFile "data/test.dat"
    let masses = map read_int $ lines contents
        read_int n = read n :: Integer
        fuel = sum $ map fuelCost masses
        rfuel = sum $ map recursiveFuelCost masses
    putStrLn $ "fuel: " ++ show fuel
    putStrLn $ "recursive fuel: " ++ show rfuel

fuelCost :: Integer -> Integer
fuelCost x = div x 3 -2

recursiveFuelCost :: Integer -> Integer
-- recursiveFuelCost x = sum $ takeWhile (>0) $ tail $ iterate fuelCost x
recursiveFuelCost = sum . takeWhile (> 0) . tail . iterate fuelCost
