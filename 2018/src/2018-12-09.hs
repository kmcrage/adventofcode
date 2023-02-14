{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow
import qualified Data.List                      as L
import qualified Data.List.PointedList.Circular as P
import qualified Data.List.Split                as L
import qualified Data.Map                       as M
import           Data.Maybe                     (fromJust)
import           Debug.Trace

type Score = M.Map Int Int

type Board = P.PointedList Int

main :: IO ()
main = do
  let -- part1 = game 9 25 -- 32
      -- part1 = game 17 1104 -- 2764
      -- part1 = game 30 5807 -- 37305
      part1 = game 441 71032 -- 393229
      part2 = game 441 7103200 -- 3273405195
  putStrLn $ "Winning Score: " ++ show part1
  putStrLn $ "Winning Score: " ++ show part2

game :: Int -> Int -> Int
game players moves = turn players moves 1 M.empty (P.singleton 0)

turn :: Int -> Int -> Int -> Score -> Board -> Int
turn players moves move score board
  | move == moves = maximum . M.elems $ score
  | mod move 23 == 0 = turn players moves (move + 1) score'' board''
  | otherwise = turn players moves (move + 1) score board'
  where
    board' = P.insertLeft move (P.moveN 2 board)
    --
    boardO = P.moveN (-7) board
    removed = P._focus boardO
    board'' = fromJust . P.deleteRight $ boardO
    score'' = M.insertWith (+) (mod move players) (move + removed) score
