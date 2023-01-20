{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore: Use second -}
import           Control.Arrow
import           Data.List.Split as S
import qualified Data.Map        as M
import           Debug.Trace

type Reagent = (String, Int)

type Reaction = (Reagent, [Reagent])

type RecipeBook = M.Map String Reaction

type Factory = M.Map String Int

main :: IO ()
main = do
  contents <- readFile "data/2019-12-14.dat"
  -- contents <- readFile "data/test.dat"
  let recipes = parse contents
      result1 = oreRequired recipes 1
      result2 = fuelCreated recipes 1000000000000
  putStrLn $ "Ore Required: " ++ show result1
  putStrLn $ "Max fuel: " ++ show result2

fuelCreated :: RecipeBook -> Int -> Int
fuelCreated recipes target = fuelBinSearch recipes target (div mx 2) mx
  where
    mx =
      head . dropWhile (\n -> oreRequired recipes n < target) $
      iterate (*2) 1

fuelBinSearch :: RecipeBook -> Int -> Int -> Int -> Int
fuelBinSearch recipes target mn mx
  | mn == mx - 1 = mn
  | ore <= target = fuelBinSearch recipes target fuel mx
  | otherwise = fuelBinSearch recipes target mn fuel
  where
    fuel = div (mn + mx) 2
    ore = oreRequired recipes fuel

oreRequired :: RecipeBook -> Int -> Int
oreRequired recipes n = requirements M.! "ORE"
  where
    factory = M.fromList [("FUEL", n)]
    requirements = process recipes factory

process :: RecipeBook -> Factory -> Factory
process recipes factory
  | null candidates = factory
  | otherwise = process recipes factory'
  where
    candidates = M.filter (> 0) $ M.delete "ORE" factory
    (cName, cAmmount) = M.findMin candidates
    reaction = recipes M.! cName
    rAmmount = snd $ fst reaction
    mult = cdiv cAmmount rAmmount
    update = map (\(n, a) -> (n, mult * a)) >>> M.fromList $ snd reaction
    update' = M.insert cName (negate (mult * rAmmount)) update
    factory' = M.unionWith (+) factory update'

cdiv :: Int -> Int -> Int
cdiv a b = negate $ div (negate a) b

parse :: String -> RecipeBook
parse =
  lines >>>
  map parseLine >>> map (\(inp, out) -> (fst inp, (inp, out))) >>> M.fromList

parseLine :: String -> Reaction
parseLine = S.splitOn " => " >>> toRecipe

toRecipe :: [String] -> Reaction
toRecipe reaction = (input, outputs)
  where
    input = toReagent $ last reaction
    outputs = head >>> S.splitOn ", " >>> map toReagent $ reaction

toReagent :: String -> Reagent
toReagent s = (head names, read num :: Int)
  where
    (num:names) = S.splitOn " " s
