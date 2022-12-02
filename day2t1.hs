import AOCUtil
import Data.Typeable

data RPS = Rock | Paper | Scissors deriving Eq

conv :: String -> RPS
conv "A" = Rock
conv "B" = Paper
conv "C" = Scissors
conv "X" = Rock
conv "Y" = Paper
conv "Z" = Scissors

moveScore :: RPS -> Int
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissors = 3

-- Score as P1
matchScore :: RPS -> RPS -> Int
matchScore Rock Scissors = 6
matchScore Paper Rock = 6
matchScore Scissors Paper = 6
matchScore a b = if a == b then 3 else 0

score :: RPS -> RPS -> Int
score a b = (moveScore a) + (matchScore a b)

scoreOfLine s = helper $ split ' ' s
    where helper (x:y:[]) = score (conv y) (conv x) -- first move is opponent's move

main = do
    raw <- readFile "day2.in"
    print $ sum $ map scoreOfLine $ lines raw