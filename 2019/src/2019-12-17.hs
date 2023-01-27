{- HLINT ignore: Use bimap -}
import           Control.Arrow
import qualified Data.Char       as C
import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Debug.Trace

import qualified IntCode         as IC

type Coord = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "data/2019-12-17.dat"
  -- contents <- readFile "data/test.dat"
  let intcode = IC.readIntcode contents
      m = IC.runPrg intcode []
      (vrPos, vrDir, pts) = parse $ IC.output m
      aligns = sumAlign $ cross pts
      route = findRoute vrPos vrDir pts []
      route' = compress route []
      message = toMessage route'
      intcode' = [2] ++ tail intcode
      dust = last . IC.output $ IC.runPrg intcode' message
  putStrLn $ "Sum of Alignments: " ++ show aligns
  putStrLn $ "Dust: " ++ show dust
  -- view dust

view :: [Int] -> IO ()
view d = putStrLn (map C.chr d) 

findRoute :: Coord -> Int -> S.Set Coord -> [String] -> [String]
findRoute (i, j) dir pts rt
  | null turns = rt
  | otherwise = findRoute pos' dir' pts rt'
  where
    dirmap = M.fromList $ zip [0 ..] directions
    turns = filter
        (\(_, d) ->
           let (di, dj) = dirmap M.! d
            in (i + di, j + dj) `S.member` pts) $
      [("R", mod (dir + 1) 4), ("L", mod (dir - 1) 4)]
    (trn, dir') = head turns
    (di, dj) = dirmap M.! dir'
    (steps, pos') =
      map (\s -> (s, (i + s * di, j + s * dj))) >>>
      takeWhile (\(_, p) -> p `S.member` pts) >>> last $
      [1 ..]
    rt' = rt ++ [trn, show steps]

toMessage :: ([String], [[String]]) -> [Int]
toMessage (ms, ws) = message'
  where
    message = map (L.intersperse ",") >>> map toNums >>> L.intersperse [10] >>> concat $ [ms] ++ ws
    message' = message ++ [10, 110, 10] -- video feed
    
toNums :: [String] -> [Int]
toNums = map (C.ord) . concat 
{-  | C.isUpper c = C.ord c
  | otherwise = read s :: Int
  where
    c = fst . head . C.readLitChar $ s -}
directions ::[Coord]
directions = [(0, -1), (1,0), (0,1), (-1, 0)]
    -- list is map C.ord ['^', '>', 'v', '<'] = [94,62,118,60]
{-

test = ["R","8","R","8","R","4","R","4","R","8","L","6","L","2","R","4","R","4","R","8","R","8","R","8","L","6","L","2"]
["A","B","C","B","A","C"], len:11
[["R","8","R","8"] len:7
,["R","4","R","4","R","8"] len:11
,["L","6","L","2"]] len:7

-}
compress :: (Ord a, Show a) => [a] -> [[a]] -> ([String], [[a]])
compress input ws
  | length ws < 3 = compress input (ws ++ [w])
  | deprefix input ws == [] = (encode input ws, ws)
  | length (last ws) <= 1 = compress input ws'
  | otherwise = compress input ws''
  where
    input' = deprefix input ws
    w = L.inits >>> takeWhile ((<= 20) . len) >>> last $ input'
    ws'' = init ws ++ [init $ last ws]
    ws' = reverse >>> dropWhile ((<= 1) . length) >>> shorten >>> reverse $ ws
    shorten (x:xs) = [init x] ++ xs

len :: Show a => [a] -> Int
len s = length (show s) - 2 * (length s) - 2

-- encode "hihihohahihiho" ["hi","ho","ha"]
encode :: Ord a => [a] -> [[a]] -> [String]
encode input words = fst >>> map (abbrev M.!) $ deprefix' input words words []
  where
    abbrev = M.fromList $ zip words ["A", "B", "C"]

deprefix :: Eq a => [a] -> [[a]] -> [a]
deprefix input words = snd $ deprefix' input words words []

-- args: input_str [all words] [untested words], result: (prefix split into words, unprefixed input)
deprefix' :: Eq a => [a] -> [[a]] -> [[a]] -> [[a]] -> ([[a]], [a])
deprefix' input words [] ps = (ps, input)
deprefix' input words (w:ws) ps
  | input' /= input = deprefix' input' words words (ps ++ [w])
  | otherwise = deprefix' input words ws ps
  where
    input' = maybe input id $ L.stripPrefix w input

cross :: S.Set Coord -> S.Set Coord
cross pts = S.filter ((== 4) . length . npts) pts
  where
    npts (i, j) =
      map (\(di, dj) -> (i + di, j + dj)) >>> filter (`S.member` pts) $ directions
    

sumAlign :: S.Set Coord -> Int
sumAlign = foldr (\(i, j) s -> i * j + s) 0

parse :: [Int] -> (Coord, Int, S.Set Coord)
parse inp = ((x, y), dir', pts)
  where
    combo = L.splitWhen (== 10) >>> zip [0 ..] >>> concatMap coordInt $ inp
    pts =
      filter (\(i, j, c) -> c == 35) >>>
      map (\(i, j, c) -> (i, j)) >>> S.fromList $
      combo
    (x, y, dir) =
      dropWhile (\(i, j, c) -> c `elem` [10, 35, 46]) >>> head $ combo
    dir' = dirs M.! dir
    dirs = M.fromList $ zip [94, 62, 118, 60] [0 ..]
    -- list is map C.ord ['^', '>', 'v', '<'] = [94,62,118,60]

coordInt :: (Int, [Int]) -> [(Int, Int, Int)]
coordInt (j, cs) = zipWith (\i c -> (i, j, c)) [0 ..] cs
