{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow ((&&&), (>>>))
import qualified Data.List      as L
import qualified Data.Map      as M
import qualified Data.Set      as S
import           Debug.Trace   (trace)

type Recipes = M.Map Int Int

main :: IO ()
main = do
  let recipes = iterate mix ((0, 1), M.fromList $ zip [0 ..] [3, 7])
      pt1 = nextScores 909441 10 recipes
      pt2 = prevScores  909441 recipes--     59414 first appears after 2018 recipes.
  putStrLn $ "Recipes At Pos: " ++ pt1
  -- putStrLn $ "Recipes Before Infix: " ++ show pt2


prevScores :: Int -> [((Int, Int), Recipes)] -> Int
prevScores num recipes = index 
  where
    infx = map (\d -> read [d] ::Int) .  show $ num
    is = M.elems . snd . head . dropWhile ( not . L.isPrefixOf infx . M.elems . snd) $ recipes
    index = lookupInfix ((div (length is) 2) - (length infx)) infx is 

lookupInfix :: Eq a => Int -> [a] -> [a] -> Int
lookupInfix n as (b:bs)
  | null bs = 0
  | L.isPrefixOf as (b:bs) = n
  | otherwise = lookupInfix (n+1) as bs

nextScores :: Int -> Int -> [((Int, Int), Recipes)] -> String
nextScores d t recipes = concat . map show $ is 
  where
    t' = d+t
    is = drop d . take t' . M.elems . snd . head . dropWhile ((<t') . length . snd) $ recipes


mix :: ((Int, Int), Recipes) -> ((Int, Int), Recipes)
mix ((e1, e2), recipes)
  | x < 10 = ((e1', e2'), recipes')
  | otherwise = ((e1'', e2''), recipes2')
  where
    elfUpdate e r = mod (e + r M.! e + 1) (length r)
    x = recipes M.! e1 + recipes M.! e2
    x' = mod x 10
    --
    recipes' = M.insert (length recipes) x' recipes
    e1' = elfUpdate e1 recipes'
    e2' = elfUpdate e2 recipes'
    --
    recipes2 = M.insert (length recipes) 1 recipes
    recipes2' = M.insert (length recipes2) x' recipes2
    e1'' = elfUpdate e1 recipes2'
    e2'' = elfUpdate e2 recipes2'
