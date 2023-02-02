{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore: Use second -}
-- cabal run --enable-profiling day16 --  +RTS -p
import           Control.Arrow

import qualified Data.Char     as C
import qualified Data.List     as L
import           Debug.Trace

import qualified IntCode       as IC

type Coord = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "data/2019-12-21.dat"
  let intcodes = IC.readIntcode contents
      part1 = run intcodes 1
      part2 = run intcodes 2
  putStrLn $ "Walk Dust: " ++ show (snd $ last part1)
  putStrLn $ "Run Dust: " ++ show (snd $ last part2)

run :: [Int] -> Int -> [(String, Int)]
run intcodes mode = out
  where
    prog = makePrograms mode
    out =
      map (\(s,p) -> (s, IC.output >>> last $ IC.runPrg intcodes p)) >>>
      filter ((> 256) . snd)  $
      prog

makePrograms :: Int -> [(String, [Int])]
makePrograms mode
  | mode == 2 = progs'
  | otherwise =  progs
  where
    msgs =
      [ ""
      , "NOT Z J\n"
      , "AND Z J\n"
      , "OR Z J\n"
      , "NOT Z T\nAND T J\n"
      , "NOT Z T\nOR T J\n"
      ]
    -- jump if the next step (A) is a hole
    -- don't jump if the 3rd next step (D) is a hole
    process pre post = map fromZ >>> concat >>> \t -> pre ++ t ++ post
    walkPost = "AND D J\nNOT A T\nOR T J\nWALK\n"
    progs = map (process "" walkPost) >>> map (id &&& map C.ord) $ 
      [[('B', mb), ('C', mc)] | mb <-msgs, mc <-msgs]
    runPre = "NOT B J\nNOT C T\nOR T J\n"
    runPost = "AND D J\nNOT A T\nOR T J\nRUN\n"
    progs' = map (process runPre runPost) >>> map (id &&& map C.ord) $ 
      [[('E', me), ('H', mh)] | me <-msgs, mh <-msgs]

choose :: Int -> [a] -> [[a]]
choose n list = concatMap L.permutations $ choose' list []
  where
    choose' [] r
      | length r == n = [r]
      | otherwise = []
    choose' (x:xs) r
      | length r == n = [r]
      | otherwise = choose' xs (x : r) ++ choose' xs r

fromZ :: (C.Char, String) -> String
fromZ (l, msg) = map tr msg
  where
    tr c
      | c == 'Z' = l
      | otherwise = c
