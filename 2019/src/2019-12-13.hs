import           Control.Arrow
import           Data.List.Split
import qualified Data.Map        as M
import           Debug.Trace

import           IntCode

type Coord = (Int, Int)

type Board = M.Map Coord Int

data Game =
  Game
    { board   :: Board
    , machine :: Machine
    , ballx   :: Int
    , paddlex :: Int
    }
  deriving (Show)

main :: IO ()
main = do
  contents <- readFile "data/2019-12-13.dat"
  -- contents <- readFile "data/test.dat"
  let intcodes = readIntcode contents
      blocks = length . M.filter (== 2) $ playGame intcodes
      games = iterate playMoveAI $ initGameAI intcodes
      final = tail >>> dropWhile hasBlocks >>> head $ games
      score = board final M.! (-1, 0)
  putStrLn $ "Num Blocks: " ++ show blocks
  putStrLn $ "AI Score: " ++ show score

hasBlocks :: Game -> Bool
hasBlocks game = not $ null blocks
  where
    b = board game
    blocks = M.filter (== 2) b

playMoveAI :: Game -> Game
playMoveAI game =
  game
    {machine = flushOutput mafter, board = b, ballx = bx', paddlex = px + pmove}
  where
    bx = ballx game
    px = paddlex game
    pmove = signum (bx - px)
    mbefore = addSignal (machine game) [pmove]
    mafter = runPrgAt mbefore
    moves = chunksOf 3 $ output mafter
    b = playGameMoves (board game) moves
    bx' = xCoord 4 b

initGameAI :: [Int] -> Game
initGameAI (ic:ics) =
  Game {machine = flushOutput m', board = b, ballx = bx, paddlex = px}
  where
    ic' = 2 : ics
    m = initMachine ic' []
    m' = runPrgAt m
    moves = chunksOf 3 $ output m'
    b = playGameMoves M.empty moves
    bx = xCoord 4 b
    px = xCoord 3 b

xCoord :: Int -> Board -> Int
xCoord n = M.filter (== n) >>> M.findMin >>> fst >>> fst

playGame :: [Int] -> Board
playGame ic = playGameMoves board moves
  where
    moves = chunksOf 3 . output $ runPrg ic []
    board = M.empty :: Board

playGameMoves :: Board -> [[Int]] -> Board
playGameMoves board [] = board
playGameMoves board ([x, y, v]:moves) = playGameMoves board' moves
  where
    board' = M.insertWith updateBoard (x, y) v board

updateBoard :: Int -> Int -> Int
updateBoard _ 1     = 1
updateBoard _ 3     = 3
updateBoard after _ = after
