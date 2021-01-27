import Data.Char
import Data.List
import System.IO

data Player = O | B | X
  deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Int -> Grid
empty size = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

diag :: Int -> Grid -> [Player]
diag size g = [g !! n !! n | n <- [0 .. size - 1]]

wins :: Int -> Player -> Grid -> Bool
wins size p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag size g, diag size (map reverse g)]

won :: Int -> Grid -> Bool
won size g = wins size O g || wins size X g

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

putGrid :: Int -> Grid -> IO ()
putGrid size = putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [replicate ((size * 4) - 1) '-']

valid :: Int -> Grid -> Int -> Bool
valid size g i = 0 <= i && i < size ^ 2 && concat g !! i == B

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

move :: Int -> Grid -> Int -> Player -> [Grid]
move size g i p = [chop size (xs ++ [p] ++ ys) | valid size g i]
  where
    (xs, B : ys) = splitAt i (concat g)

nxtTree :: Grid -> Tree Grid -> Tree Grid
nxtTree g (Node g' ts') = head [Node g'' ts'' | Node g'' ts'' <- ts', g'' == g]

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ "H")

prompt :: Player -> String
prompt p = "Player" ++ show p ++ ", enter your move: "

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt

run' :: Int -> Grid -> Player -> IO ()
run' size g p
  | wins size O g = putStrLn "Player O wins!\n"
  | wins size X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | otherwise = do
    i <- getNat (prompt p)
    case move size g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        run' size g p
      [g'] -> run size g' (next p)

run :: Int -> Grid -> Player -> IO ()
run size g p = do
  cls
  goto (1, 1)
  putGrid size g
  run' size g p

tictactoe :: IO ()
tictactoe = do
  size <- getSize
  run size (empty size) O

data Tree a = Node a [Tree a]
  deriving (Show)

moves :: Int -> Grid -> Player -> [Grid]
moves size g p
  | won size g = []
  | full g = []
  | otherwise = concat [move size g i p | i <- [0 .. ((size ^ 2) - 1)]]

gametree :: Int -> Grid -> Player -> Tree Grid
gametree size g p = Node g [gametree size g' (next p) | g' <- moves size g p]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Int -> Tree Grid -> Tree (Grid, Player)
minimax size (Node g [])
  | wins size O g = Node (g, O) []
  | wins size X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax size (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map (minimax size) ts
    ps = [p | Node (_, p) _ <- ts']

min' :: Int
min' = -2

max' :: Int
max' = 2

bestTree :: Player -> (Int, Grid) -> (Int, Grid) -> (Int, Grid)
bestTree p res now
  | p == O =
    if fst res < fst now
      then res
      else now
  | otherwise =
    if fst res > fst now
      then res
      else now

alphaBetaPrune' :: Int -> Player -> Int -> Int -> (Int, Grid) -> [Tree Grid] -> (Int, Grid)
alphaBetaPrune' _ _ _ _ best [] = best
alphaBetaPrune' size p a b best ts
  | a >= b = best
  | p == O = alphaBetaPrune' size p a b' best' (tail ts)
  | otherwise = alphaBetaPrune' size p a' b best' (tail ts)
  where
    Node g' ts' = head ts
    res = alphaBetaPrune size a b (Node g' ts')
    best' = bestTree p (fst res, g') best
    a' = max a (fst best')
    b' = min b (fst best')

alphaBetaPrune :: Int -> Int -> Int -> Tree Grid -> (Int, Grid)
alphaBetaPrune size a b (Node g [])
  | wins size O g = (-1, g)
  | wins size X g = (1, g)
  | otherwise = (0, g)
alphaBetaPrune size a b (Node g ts)
  | turn g == O = alphaBetaPrune' size O a b (max', g) ts
  | turn g == X = alphaBetaPrune' size X a b (min', g) ts

bestmove :: Int -> Tree Grid -> Player -> Grid
bestmove size (Node g ts) p = snd (alphaBetaPrune size min' max' (Node g ts))

play' :: Int -> Tree Grid -> Player -> IO ()
play' size (Node g ts) p
  | wins size O g = putStrLn "Player O wins!\n"
  | wins size X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
    i <- getNat (prompt p)
    case move size g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        play' size (Node g ts) p
      [g'] -> play size (nxtTree g' (Node g ts)) (next p)
  | p == X = do
    putStr "Player X is thinking... "
    (play size $! nxtTree (bestmove size (Node g ts) p) (Node g ts)) (next p)

play :: Int -> Tree Grid -> Player -> IO ()
play size (Node g ts) p = do
  cls
  goto (1, 1)
  putGrid size g
  play' size (Node g ts) p

fstPlayer :: IO Player
fstPlayer = do
  putStr "First Player: "
  x <- getLine
  if x == "O"
    then return O
    else
      if x == "X"
        then return X
        else do
          print "ERROR: Invalid player"
          fstPlayer

getSize :: IO Int
getSize = do
  putStr "Length of winning line: "
  xs <- getLine
  if all isDigit xs
    then return (read xs)
    else do
      print "ERROR: Invalid length"
      getSize

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  p <- fstPlayer
  size <- getSize
  play size (gametree size (empty size) p) p
