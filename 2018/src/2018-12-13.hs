{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow   ((&&&), (>>>))
import qualified Data.Char       as C
import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Debug.Trace     (trace)
import           Prelude         hiding (Left)

type Track = M.Map Coord C.Char

type Coord = (Int, Int)

data Cart =
  Cart
    { position  :: Coord
    , direction :: Direction
    , turn      :: Turn
    }
  deriving (Eq, Show)

instance Ord Cart where
  compare (Cart (i, j) _ _) (Cart (k, l) _ _) = compare (j, i) (l, k)

type State = (Crashes, [Cart])

data Turn
  = Left
  | Straight
  | Right
  deriving (Show, Eq, Enum)

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Enum)

type Crashes = S.Set Coord

main :: IO ()
main = do
  contents <- readFile "data/2018-12-13.dat"
  -- contents <- readFile "data/test.dat"
  let (track, carts) = parse contents
      ticks = iterate (tick track) (S.empty, carts)
      crash = dropWhile (null . fst) >>> head >>> fst >>> S.findMin $ ticks
      final =
        dropWhile ((> 1) . length . snd) >>> head >>> snd >>> head >>> position $
        ticks
  putStrLn $ "First Crash Site: " ++ show crash
  putStrLn $ "Final Position: " ++ show final

tick :: Track -> State -> State
tick track (crashes, carts) = (allCrashes, moved')
  where
    ps = map (position) >>> S.fromList $ carts
    moved =
      L.sort >>>
      L.scanl' (tick' track) ((ps, S.empty), Cart (0, 0) North Left) >>> drop 1 $
      carts -- move and crash
    ((_, allCrashes), _) = last $ moved
    moved' = map snd >>> filter ((`S.notMember` allCrashes) . position) $ moved

tick' ::
     Track -> ((Crashes, Crashes), Cart) -> Cart -> ((Crashes, Crashes), Cart)
tick' track ((ps, crashes), _) cart
  | S.member c crashes = ((ps', crashes), cart)
  | S.member c' ps = ((ps', crashes'), cart')
  | otherwise = ((ps', crashes), cart')
  where
    c = position cart
    d = direction cart
    t = turn cart
    c' = move c d
    (d', t') = dirn track c' d t
    hit = S.member c' ps
    ps' = S.delete c >>> S.insert c' $ ps
    crashes' = S.insert c' crashes
    cart' = cart {position = c', direction = d', turn = t'}

move :: Coord -> Direction -> Coord
move (i, j) d
  | d == North = (i, j - 1)
  | d == South = (i, j + 1)
  | d == East = (i + 1, j)
  | d == West = (i - 1, j)

dirn :: Track -> Coord -> Direction -> Turn -> (Direction, Turn)
dirn track c d t
  | ct == '+' = (d', t')
  | d == North && ct == '/' = (East, t)
  | d == North && ct == '\\' = (West, t)
  | d == South && ct == '/' = (West, t)
  | d == South && ct == '\\' = (East, t)
  | d == East && ct == '/' = (North, t)
  | d == East && ct == '\\' = (South, t)
  | d == West && ct == '/' = (South, t)
  | d == West && ct == '\\' = (North, t)
  | otherwise = (d, t)
  where
    ct = track M.! c
    d' = toEnum $ mod (fromEnum d + fromEnum t - 1) 4
    t' = toEnum $ mod (fromEnum t + 1) 3

parse :: String -> (Track, [Cart])
parse input = (track, carts)
  where
    track =
      lines >>>
      zip [0 ..] >>>
      concatMap coordChar >>>
      filter (\(i, j, c) -> c /= ' ') >>>
      map (\(i, j, c) -> ((i, j), c)) >>> M.fromList $
      input
    coordChar (j, line) = zipWith (\i char -> (i, j, char)) [0 ..] line
    carts = M.filter (`L.elem` ">^<v") >>> M.toList >>> map toCart $ track

toDir :: Char -> Direction
toDir d
  | d == '^' = North
  | d == '>' = East
  | d == 'v' = South
  | d == '<' = West

toCart :: (Coord, C.Char) -> Cart
toCart (coord, chr) =
  Cart {position = coord, direction = toDir chr, turn = Left}
