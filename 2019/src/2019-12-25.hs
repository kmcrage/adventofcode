{-# LANGUAGE BlockArguments #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow
import qualified Data.Char       as C
import qualified Data.List       as L
import           Data.List.Split as L
import qualified Data.Map        as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import           Debug.Trace

import qualified Data.Set        as S
import qualified IntCode         as IC

type Queue = [State]

data Ship =
  Ship
    { itemLocs  :: M.Map String String -- item, path
    , locations :: M.Map String [String] -- loc, path
    , blacklist :: S.Set String
    }
  deriving (Show, Eq)

data State =
  State
    { path      :: [String]
    , machine   :: IC.Machine
    , inventory :: S.Set String
    }
  deriving (Show)

data Room =
  Room
    { name  :: String
    , items :: [String]
    , doors :: [String]
    }
  deriving (Show, Eq)

main :: IO ()
main = do
  contents <- readFile "data/2019-12-25.dat"
  let intcodes = IC.readIntcode contents
      ship = explore intcodes
      state = collect intcodes ship
      password = pressurePlate ship "Security Checkpoint" state
  putStrLn $ "rooms: " ++ show (M.keysSet (itemLocs ship))
  putStrLn $ "items: " ++ show (inventory state)
  putStrLn $ "password: " ++ password

explore :: [Int] -> Ship
explore intcodes = ship'
  where
    m = IC.initMachine intcodes []
    state = State {machine = m, path = [], inventory = S.empty}
    ship =
      Ship
        { itemLocs = M.empty
        , locations = M.empty
        , blacklist =
            S.fromList
              [ "giant electromagnet"
              , "escape pod"
              , "infinite loop"
              , "molten lava"
              , "photons"
              ]
        }
    ship' = bfs ship [state]

directions :: M.Map String String
directions =
  M.fromList
    [("north", "south"), ("south", "north"), ("east", "west"), ("west", "east")]

room :: State -> Room
room state
  | null items = r
  | otherwise = r'
  where
    dirns = M.keysSet directions
    msg =
      machine >>>
      IC.flushOutput >>>
      IC.runPrgAt >>> IC.output >>> toAscii >>> L.splitOn "==" $
      state
    msg' = drop ((length msg) - 2) msg
    name = head >>> T.pack >>> T.strip >>> T.unpack $ msg'
    doors =
      last >>>
      T.pack >>> T.words >>> map T.unpack >>> filter (\w -> S.member w dirns) $
      msg'
    r = Room {name = name, doors = doors, items = []}
    items = last >>> L.splitOn "Items here:" >>> drop 1 $ msg'
    items' =
      last >>> lines >>> filter (L.isPrefixOf "- ") >>> map (drop 2) $ items
    r' = Room {name = name, doors = doors, items = items'}

bfs :: Ship -> Queue -> Ship
bfs ship [] = ship
bfs ship (state:queue)
  | M.member l ls = bfs ship queue
  | otherwise = bfs ship' (queue ++ moves)
    -- explore room
  where
    rm = room state
    l = name rm
    is = items rm
    ls = locations ship
    ils = itemLocs ship
    -- update ship
    pth = path state
    ls' = M.insert l pth ls
    is' = L.foldl' (\m i -> M.insert i l m) ils is
    ship' = ship {locations = ls', itemLocs = is'}
    -- go through doors
    moves = map (move state pth) $ doors rm

move :: State -> [String] -> String -> State
move state pth dr = state'
  where
    m = machine >>> IC.flushOutput $ state
    m' = fromAscii >>> IC.addSignal m $ dr ++ "\n"
    state' = state {machine = m', path = pth ++ [dr]}

collect :: [Int] -> Ship -> State
collect intcodes ship = state'
  where
    m = IC.initMachine intcodes []
    state = State {machine = m, path = [], inventory = S.empty}
    state' = getItem ship state (M.keys $ itemLocs ship)

getItem :: Ship -> State -> [String] -> State
getItem ship state [] = state
getItem ship state (item:queue)
  | S.member item bl = getItem ship state queue
  | otherwise = getItem ship state' queue
  where
    bl = blacklist ship
    ils = itemLocs ship
    ls = locations ship
    steps = ls M.! (ils M.! item)
    stepsReturn = L.reverse >>> map (directions M.!) $ steps
    steps' = steps ++ ["take " ++ item] ++ stepsReturn
    steps'' = map (\m -> fromAscii (m ++ "\n")) steps'
    m' =
      L.foldl'
        (\m s ->
           let m' = IC.addSignal m s
            in IC.runPrgAt m')
        (machine state)
        steps''
    msg = IC.output >>> toAscii $ m'
    state' = state {machine = m', inventory = S.insert item (inventory state)}

pressurePlate :: Ship -> String -> State -> String
pressurePlate ship rm state = password
  where
    ls = locations ship
    steps = map (\m -> fromAscii (m ++ "\n")) $ ls M.! rm
    m = L.foldl' run (machine state) steps
    -- test is west of this room, powerset is not efficient here 
    tests =
      S.powerSet >>>
      S.toList >>>
      map toDrop >>>
      map (\d -> d ++ ["west"]) >>> map (map (\m -> fromAscii (m ++ "\n"))) $
      inventory state
    toDrop = S.toList >>> map (\i -> "drop " ++ i)
    run m s =
      let m' = IC.addSignal m s
       in IC.runPrgAt m'
    password  =
      map (L.foldl' run m) >>>
      dropWhile
        (\m -> IC.output >>> toAscii >>> L.isInfixOf "ejected back to the" $ m) >>>
      head >>> IC.output >>> toAscii >>> words >>> filter (all C.isDigit) >>> head $
      tests

toAscii :: [Int] -> String
toAscii = map C.chr

fromAscii :: String -> [Int]
fromAscii = map C.ord
