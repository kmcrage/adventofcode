{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow        ((&&&), (>>>))
import qualified Data.Bits            as B
import qualified Data.List            as L
import qualified Data.List.Split      as L
import qualified Data.Map             as M
import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Set             as S
import           Debug.Trace          (trace)

type Coord = (Int, Int)

data Direction
  = Across
  | Down
  deriving (Show, Eq, Ord)

data State
  = Empty
  | Still
  | Flowing
  | Clay
  deriving (Show, Eq)

type Ground = M.Map Coord State

main :: IO ()
main
  -- contents <- readFile "data/test.dat"
 = do
  contents <- readFile "data/2018-12-17.dat"
  let ground = parse contents
      ground' = fill ground
      pt1 = length $ M.filter (/= Clay) ground'
      pt2 = length $ M.filter (== Still) ground'
  putStrLn $ "all water: " ++ show pt1
  putStrLn $ "still water: " ++ show pt2

fill :: Ground -> Ground
fill ground = flow ground imx $ Q.singleton (imn, 500) Down
  where
    imn = minimum . map fst . M.keys $ ground
    imx = maximum . map fst . M.keys $ ground

flowDown :: Ground -> Int -> Q.MaxPQueue Coord Direction -> Coord -> Ground
flowDown ground imx queue (i, j)
  | i >= imx = flow ground imx queue
  | null streamD = flow ground imx queue
  | isBottom = flow ground' imx queue
  | otherwise = flow ground' imx queueA
  where
    streamD =
      map (\n -> (i + n, j)) >>>
      L.takeWhile
        (\c -> M.findWithDefault Flowing c ground == Flowing && fst c <= imx) $
      [0 ..]
    coordD = last streamD
    isBottom = fst coordD == imx
    ground' = L.foldl' (\m c -> M.insert c Flowing m) ground streamD
    queueA = Q.union queue $ Q.fromList [(coordD, Across)]

flowAcross :: Ground -> Int -> Q.MaxPQueue Coord Direction -> Coord -> Ground
flowAcross ground imx queue (i, j)
  | rCell == Clay && lCell == Clay = flow groundS imx queueU
  | rCell == Clay && lCell == Empty = flow groundF imx queueL
  | rCell == Clay = flow groundF imx queue
  | lCell == Clay && rCell == Empty = flow groundF imx queueR
  | lCell == Clay = flow groundF imx queue
  | lCell == Empty && rCell == Empty = flow groundF imx queueLR
  | lCell == Empty = flow groundF imx queueL
  | rCell == Empty = flow groundF imx queueR
  | otherwise = flow groundF imx queue
  where
    supported (i, j) =
      M.findWithDefault Flowing (i, j) ground /= Clay &&
      M.findWithDefault Flowing (i + 1, j) ground /= Flowing
    (streamR, streamR') = map (\n -> (i, j + n)) >>> L.span supported $ [0 ..]
    (streamL, streamL') = map (\n -> (i, j - n)) >>> L.span supported $ [0 ..]
    coordR' = head streamR'
    coordL' = head streamL'
    rCell = M.findWithDefault Empty coordR' ground
    lCell = M.findWithDefault Empty coordL' ground
    --
    groundS = L.foldl' (\m c -> M.insert c Still m) ground $ streamR ++ streamL
    groundF =
      L.foldl' (\m c -> M.insert c Flowing m) ground $ streamR ++ streamL
    --
    queueU = Q.insert (i - 1, j) Down queue
    queueL = Q.insert coordL' Down queue
    queueR = Q.insert coordR' Down queue
    queueLR = Q.insert coordR' Down queueL

flow :: Ground -> Int -> Q.MaxPQueue Coord Direction -> Ground
flow ground imx queue
  | null queue = ground
  | dir == Down = flowDown ground imx queue' c
  | dir == Across = flowAcross ground imx queue' c
  where
    ((c, dir), queue') = Q.deleteFindMax queue

parse :: String -> Ground
parse input = L.foldl' parseLine M.empty $ lines input

parseLine :: Ground -> String -> Ground
parseLine g line
  | i == "x" = L.foldl' (\m y -> M.insert (y, iN) Clay m) g [jmn .. jmx]
  | i == "y" = L.foldl' (\m x -> M.insert (iN, x) Clay m) g [jmn .. jmx]
  where
    coordA:coordR:_ = L.splitOn ", " line
    i:iStr:_ = L.splitOn "=" coordA
    j:range:_ = L.splitOn "=" coordR
    mnS:mxS:_ = L.splitOn ".." range
    iN = read iStr :: Int
    jmn = read mnS :: Int
    jmx = read mxS :: Int

view :: Ground -> String
view ground = map toChr [(i, j) | i <- [imn .. imx], j <- [jmn .. jmx + 1]]
  where
    (imn, imx) = (minimum &&& maximum) . map fst . M.keys $ ground
    (jmn, jmx) = (minimum &&& maximum) . map snd . M.keys $ ground
    toChr (i, j)
      | (i, j) == (imn, 500) = '+'
      | j == jmx + 1 = '\n'
      | M.notMember (i, j) ground = '.'
      | ground M.! (i, j) == Still = '~'
      | ground M.! (i, j) == Flowing = '|'
      | otherwise = '#' -- Clay
