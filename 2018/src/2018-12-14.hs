{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow ((&&&), (>>>))
import qualified Data.List     as L
import qualified Data.Sequence as S
import           Debug.Trace   (trace)

main :: IO ()
main = do
  let recipes = 3 : 7 : elves (0, 1) (S.fromList [3, 7])
      pt1 = nextScores 2018 10 recipes
      pt2 = prevScores 909441 recipes
  putStrLn $ "Recipes At Pos: " ++ pt1
  putStrLn $ "Recipes Before Infix: " ++ show pt2

nextScores :: Show a => Int -> Int -> [a] -> String
nextScores d t = concat . map show . take t . drop d

lookupInfix :: Eq a => Int -> [a] -> [a] -> Int
lookupInfix n as bs
  | null bs = 0
  | L.isPrefixOf as bs = n
  | otherwise = lookupInfix (n + 1) as (tail bs)

prevScores :: Int -> [Int] -> Int
prevScores num recipes = index
  where
    infx = map (\d -> read [d] :: Int) . show $ num
    index = lookupInfix 0 infx recipes

elves :: (Int, Int) -> S.Seq Int -> [Int]
elves (e1, e2) recipes = ds ++ elves (e1', e2') recipes'
  where
    re1 = S.index recipes e1
    re2 = S.index recipes e2
    ds = digits $ re1 + re2
    recipes' = recipes S.>< S.fromList ds
    e1' = mod (e1 + re1 + 1) (length recipes')
    e2' = mod (e2 + re2 + 1) (length recipes')

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = [1, n - 10]
