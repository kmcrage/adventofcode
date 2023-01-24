{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore: Use second -}
-- cabal run --enable-profiling day16 --  +RTS -p
import           Control.Arrow

import qualified Data.List       as L
import           Data.List.Split as S
import qualified Data.Map        as M
import           Debug.Trace

main :: IO ()
main = do
  contents <- readFile "data/2019-12-16.dat"
  -- contents <- readFile "data/test.dat"
  let input = parse contents
      ffts = iterate fft input
      partOne = showInts 8 $ ffts !! 100
      offset = read (take 7 contents) :: Int
      input' = replicate 10000 >>> concat >>> drop offset $ input
      ffts' = iterate fft' input'
      partTwo = showInts 8 $ ffts' !! 100
  putStrLn $ "Part One: " ++ partOne
  putStrLn $ "Part Two: " ++ partTwo ++ "  offset: " ++ show offset

-- fft' only works on the second half of the input
fft' :: [Int] -> [Int]
fft' = init . scanr (\r a -> abs (a + r) `mod` 10) 0

showInts :: Int -> [Int] -> String
showInts m = take m >>> concatMap show

parse :: String -> [Int]
parse = S.splitOn "" >>> tail >>> map (\n -> read n :: Int)

absdigit :: Int -> Int
absdigit n = mod (abs n) 10

fft :: [Int] -> [Int]
fft input =
  map
    (\n -> absdigit . sum $ zipWith (*) input (phases n base))
    [1 .. length input]
  where
    base = [0, 1, 0, -1]

phases :: Int -> [Int] -> [Int]
phases n = tail . cycle . concatMap (replicate n)
{-
fft :: [Int] -> [Int]
fft input = map (fftsum len acc) [1 .. len]
  where
    len = length input
    acc = inputAcc input

inputAcc :: [Int] -> M.Map Int Int
inputAcc input =
  L.foldl' (\acc n -> acc ++ [n + last acc]) [0] >>> zip [0 ..] >>> M.fromList $
  input ++ replicate (length input) 0

fftsum :: Int -> M.Map Int Int -> Int -> Int
fftsum len s n = absdigit $ sum zs
  where
    base = [0, 1, 0, -1]
    base' = drop 1 $ cycle base
    mx = len `div` n
    -- accumulation map [extended out]
    zs =
      zipWith
        (\i b -> b * (s M.! (n * (i + 1) - 1) - s M.! (n * i - 1)))
        [1 .. mx]
        base'
-}
