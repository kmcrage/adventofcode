import           Data.List.Split
import qualified Data.Map        as M
import           Debug.Trace

import           IntCode

type Coord = (Int, Int)

type Dir = Int

type Hull = M.Map Coord Int

main :: IO ()
main = do
  contents <- readFile "data/2019-12-11.dat"
  let intcodes = map read_int strcodes
      read_int n = read n :: Int
      strcodes = splitOn "," contents
      part_one = length $ getHull intcodes 0
      hull = getHull intcodes 1
  putStrLn $ "Result One: " ++ show part_one
  print "Result Two:"
  printHull hull

printHull :: Hull -> IO ()
printHull hull = do
  let xs = map fst $ M.keys hull
      ys = map snd $ M.keys hull
      hmaxx = maximum xs
      hmaxy = maximum ys
      hminx = minimum xs
      hminy = minimum ys
      start = (hminx, hmaxy)
      end = (hmaxx + 1, hminy)
  printHullChar start end hminx (hmaxx + 1) hull

printHullChar :: Coord -> Coord -> Int -> Int -> Hull -> IO ()
printHullChar p end xmin xmax hull
  | p == end = putStrLn ""
  | fst p == xmax = do
    putStrLn ""
    printHullChar (xmin, snd p - 1) end xmin xmax hull
  | M.findWithDefault 0 p hull == 1 = do
    putStr "#"
    printHullChar (1 + fst p, snd p) end xmin xmax hull
  | otherwise = do
    putStr " "
    printHullChar (1 + fst p, snd p) end xmin xmax hull

getHull :: [Int] -> Int -> Hull
getHull ics tile = runRobot p d m hull
  where
    m = initMachine ics []
    p = (0, 0)
    hull = M.fromList [(p, tile)]
    d = 1

runRobot :: Coord -> Dir -> Machine -> Hull -> Hull
runRobot p d m hull
  | state m == Halted = hull
  | otherwise = runRobot p' d' m' hull'
  where
    tile = M.findWithDefault 0 p hull
    mi = addSignal m [tile]
    mrun = runPrgAt mi
    out = output mrun
    hull' = paint (head out) p hull
    d' = turn (last out) d
    p' = move p d'
    m' = flushOutput mrun

paint :: Int -> Coord -> Hull -> Hull
paint p c = M.insert c p

turn :: Int -> Dir -> Dir
turn 0 d = mod (d - 1) 4
turn _ d = mod (d + 1) 4

move :: Coord -> Dir -> Coord
move (a, b) d
  | d == 0 = (a - 1, b)
  | d == 1 = (a, b + 1)
  | d == 2 = (a + 1, b)
  | otherwise = (a, b - 1)
