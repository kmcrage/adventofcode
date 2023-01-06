
main :: IO ()
main = do  
    contents <- readFile "2019-12-01.dat"
    -- contents <- readFile "test.dat"
    let masses = map read_int $ lines contents
        read_int n = read n :: Integer
        fuel = sum $ map fuelCost masses
        rfuel = sum $ map recursiveFuelCost masses
    print $ "fuel: " ++ show fuel
    print $ "recursive fuel: " ++ show rfuel

fuelCost :: Integer -> Integer
fuelCost x = div x 3 -2

recursiveFuelCost :: Integer -> Integer
recursiveFuelCost x = sum $ takeWhile (>0) $ iterate fuelCost x

