{-# LANGUAGE TypeApplications #-}

{- HLINT ignore: Use bimap -}
import           Control.Arrow   ((&&&), (>>>))
import qualified Data.Bits       as B
import qualified Data.List       as L
import qualified Data.List.Split as L
import qualified Data.Map        as M
import qualified Data.Set        as S
import           Debug.Trace     (trace)
import qualified GHC.Enum        as E

data Op
  = Addr
  | Addi
  | Mulr
  | Muli
  | Banr
  | Bani
  | Borr
  | Bori
  | Setr
  | Seti
  | Gtri
  | Gtir
  | Gtrr
  | Eqir
  | Eqri
  | Eqrr
  deriving (Enum, Bounded, Ord, Eq, Show)

type Sample = ([Int], [Int], [Int])

type OpMap = M.Map Int Op

main :: IO ()
main
  -- contents <- readFile "data/test.dat"
 = do
  contents <- readFile "data/2018-12-16.dat"
  let (samples, prg) = parse contents
      pt1 = matches 3 samples
      ops = opMap samples
      pt2 = runPrg ops [0, 0, 0, 0] prg
  putStrLn $ ">=3 Matches: " ++ show pt1
  putStrLn $ "OpMap: " ++ show ops
  putStrLn $ "Result: " ++ show pt2

runPrg :: OpMap -> [Int] -> [[Int]] -> [Int]
runPrg _ reg [] = reg
runPrg ops reg (p:prg) = runPrg ops reg' prg
  where
    (op:a:b:c:_) = p
    reg' = runOp (ops M.! op) (a : b : c : []) reg

opMap :: [Sample] -> OpMap
opMap samples = opMap' M.empty intToOps
  where
    intToOps = L.foldr process M.empty samples
    process (b, op:args, a) =
      M.insertWith (S.intersection) op (testAllOps (b, op : args, a))

opMap' :: M.Map Int Op -> M.Map Int (S.Set Op) -> M.Map Int Op
opMap' opmap iop
  | null iop = opmap
  | otherwise = opMap' opmap' iop'
  where
    (singles, mults) = M.partition ((== 1) . length) iop
    defOps = L.foldl' S.union S.empty . M.elems $ singles
    iop' = M.map (`S.difference` defOps) mults
    opmap' = M.union opmap $ M.map S.findMin singles

matches :: Int -> [Sample] -> Int
matches n = length . filter (>= n) . map (length . testAllOps)

testAllOps :: Sample -> S.Set Op
testAllOps (before, (op:a:b:c:_), after) =
  S.fromList . filter (testOp (a : b : c : []) before after) $
  [minBound .. maxBound]

testOp :: [Int] -> [Int] -> [Int] -> Op -> Bool
testOp (a:b:c:_) before after op = runOp op (a : b : [c]) before == after

runOp :: Op -> [Int] -> [Int] -> [Int]
runOp op (a:b:c:_) registers
  | op == Addr = replaceAt registers c $ registers L.!! a + registers L.!! b
  | op == Addi = replaceAt registers c $ registers L.!! a + b
  | op == Mulr = replaceAt registers c $ registers L.!! a * registers L.!! b
  | op == Muli = replaceAt registers c $ registers L.!! a * b
  | op == Banr = replaceAt registers c $ registers L.!! a B..&. registers L.!! b
  | op == Bani = replaceAt registers c $ registers L.!! a B..&. b
  | op == Borr = replaceAt registers c $ registers L.!! a B..|. registers L.!! b
  | op == Bori = replaceAt registers c $ registers L.!! a B..|. b
  | op == Setr = replaceAt registers c $ registers L.!! a
  | op == Seti = replaceAt registers c a
  | op == Gtri && registers L.!! a > b = replaceAt registers c 1
  | op == Gtir && a > registers L.!! b = replaceAt registers c 1
  | op == Gtrr && registers L.!! a > registers L.!! b = replaceAt registers c 1
  | op == Eqri && registers L.!! a == b = replaceAt registers c 1
  | op == Eqir && a == registers L.!! b = replaceAt registers c 1
  | op == Eqrr && registers L.!! a == registers L.!! b = replaceAt registers c 1
  | otherwise = replaceAt registers c 0

replaceAt :: [a] -> Int -> a -> [a]
replaceAt list i newElement = pre ++ newElement : post
  where
    (pre, a:post) = L.splitAt i list

parse :: String -> ([Sample], [[Int]])
parse input = (samples, prog)
  where
    (sinput:pinput:_) = L.splitOn "\n\n\n" input
    samples = parseSamples $ lines sinput
    prog = parseProg $ lines pinput

parseProg :: [String] -> [[Int]]
parseProg = map (map read . L.splitOn " ") . filter (/= "")

parseSamples :: [String] -> [Sample]
parseSamples [] = []
parseSamples (line:lines)
  | head tokensBefore == "" = parseSamples lines
  | head tokensBefore == "Before" =
    (before, ops, after) : parseSamples (drop 2 lines)
  | otherwise = []
  where
    tokensBefore = L.splitOn ":" line
    before = read (last tokensBefore) :: [Int]
    ops = map (read @Int) . L.splitOn " " . head $ lines
    tokensAfter = L.splitOn ":" . head . drop 1 $ lines
    after = read (last tokensAfter) :: [Int]
