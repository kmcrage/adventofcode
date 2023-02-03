{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

{- HLINT ignore: Use second -}
-- cabal run --enable-profiling day16 --  +RTS -p
import           Control.Arrow

import qualified Data.List     as L
import qualified Data.Map      as M
import           Data.Modular
import           Debug.Trace

type Deck = M.Map Int Int

type IntM = Mod Integer (119315717514047)
type Shuffle = (IntM, IntM) -- multiplier, offset: so start as (1,0, _)

main :: IO ()
main = do
  contents <- readFile "data/2019-12-22.dat"
  -- contents <- readFile "data/test.dat"
  let cmds = lines contents
      deck = processCmds cmds (newDeck 10007)
      part1 = deck M.! 2019
      shuffle = processCmds2 cmds newShuffle -- 119315717514047
      part2 = cardFrom shuffle 101741582076661 2020
  putStrLn $ "Part1: Card 2019 is at position: " ++ show part1
  putStrLn $ "Part2: Card at position 2020 is: " ++ show part2 

newShuffle :: Shuffle
newShuffle = (1, 0) 

cardAt :: Shuffle -> Int -> IntM -> IntM
cardAt (m, o) n c = m' * c + o'
  where
    m' = m ^^ n
    o' = (m ^^ n - 1) * o / (m - 1)

cardFrom :: Shuffle -> Int -> IntM -> IntM
cardFrom (m, o) n c = (c - o') / m'
  where
    m' = m ^^ n
    o' = (m ^^ n - 1) * o / (m - 1)

processCmds2 :: [String] -> Shuffle -> Shuffle
processCmds2 cmds deck = L.foldl' processCmd2 deck cmds

processCmd2 :: Shuffle -> String -> Shuffle
processCmd2 (m, off) cmd
  | ws !! 0 == "cut" = (m, off - n1)
  | ws !! 2 == "new" = (-m, -off - 1)
  | otherwise = (m * n3, off * n3)
  where
    ws = words cmd
    n1 = read (ws !! 1)
    n3 = read (ws !! 3)

processCmds :: [String] -> Deck -> Deck
processCmds cmds deck = L.foldl' processCmd deck cmds

processCmd :: Deck -> String -> Deck
processCmd deck cmd
  | ws !! 0 == "cut" = cut n1 deck
  | ws !! 2 == "new" = newStack deck
  | otherwise = increment n3 deck
  where
    ws = words cmd
    n1 = read (ws !! 1) :: Int
    n3 = read (ws !! 3) :: Int

newDeck :: Int -> Deck
newDeck n = map (id &&& id) >>> M.fromList $ [0 .. (n - 1)]

view :: Deck -> String
view = M.toList >>> map (\(a, b) -> (b, a)) >>> L.sort >>> map snd >>> show

find :: Int -> Deck -> Int
find n = M.filter (== n) >>> M.findMin >>> fst

newStack :: Deck -> Deck
newStack deck = M.map (\c -> l - c - 1) deck
  where
    l = length deck

increment :: Int -> Deck -> Deck
increment n deck = M.map (\c -> mod (c * n) l) deck
  where
    l = length deck

cut :: Int -> Deck -> Deck
cut n deck = M.map (\c -> mod (c - n) l) deck
  where
    l = length deck
