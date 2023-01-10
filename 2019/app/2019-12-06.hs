
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

type Orbits = M.Map String String
type Path = S.Set String

main :: IO ()
main = do  
    contents <- readFile "data/2019-12-06.dat"
    -- contents <- readFile "data/test.dat"
    let parsed = map parse $ lines contents
        orbits = makeOrbits parsed
    putStrLn $ "part one: " ++ show (part1 orbits)
    putStrLn $ "part two: " ++ show (part2 orbits)

parse :: String -> (String, String)
parse text = (T.unpack planet, T.unpack moon)
    where
        split = T.splitOn (T.pack ")") (T.pack text)
        planet = split !! 0
        moon = split !! 1


part1 :: Orbits -> Int
part1 orbits = sum $ map (orbitCount orbits) $ M.keys orbits

part2 :: Orbits -> Int
part2 orbits = length youPathOnly + length sanPathOnly
    where 
        youPath = tracepath orbits S.empty (orbits M.! "YOU")
        sanPath = tracepath orbits S.empty (orbits M.! "SAN")
        youPathOnly = youPath S.\\ sanPath
        sanPathOnly = sanPath S.\\ youPath

tracepath :: Orbits -> Path -> String -> Path
tracepath orbits path moon
    | M.member moon orbits = tracepath orbits path' (orbits M.! moon)
    | otherwise            = path'
    where
        path' = S.insert moon path


makeOrbits :: [(String, String)] -> Orbits
makeOrbits = foldl' makeOrbit M.empty

makeOrbit :: Orbits -> (String, String) -> Orbits
makeOrbit orbits (planet, moon) = M.insert moon planet orbits

orbitCount :: Orbits -> String -> Int
orbitCount orbits moon 
    | M.member moon orbits = 1 + orbitCount orbits (orbits M.! moon)
    | otherwise            = 0