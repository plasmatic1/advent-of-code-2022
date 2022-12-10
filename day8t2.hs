import Data.List
import AOCUtil

parse :: [String] -> [[Int]]
parse [] = []
parse (row:xs) = (map (\c -> read [c]) row):(parse xs)

-- finds the first index of a list that satisfies the given predicate (or the last possible index of the list if none could be found, returning -1 on an empty list)
findFirst :: (a -> Bool) -> [a] -> Int
findFirst f xs = helper f xs 0
    where
        helper _ [] i = i - 1
        helper f (x:xs) i = if f x then i else helper f xs (i + 1)

score :: [[Int]] -> [[Int]] -> (Int, Int) -> Int
score g trg (x, y) = left * right * up * down
    where
        target = (g !! x) !! y
        left = 1 + (findFirst (>=target) $ reverse $ take y $ g !! x)
        right = 1 + (findFirst (>=target) $ drop (y + 1) $ g !! x)
        up = 1 + (findFirst (>=target) $ reverse $ take x $ trg !! y)
        down = 1 + (findFirst (>=target) $ drop (x + 1) $ trg !! y)

solve :: [String] -> Int
solve rawGrid = maximum $ map (score parsed (transpose parsed)) [(r, c) | r <- [0..n-1], c <- [0..m-1]]
    where
        parsed = parse rawGrid
        n = length rawGrid
        m = length (head rawGrid)

main = do
    raw <- readFile "day8.in"
    print $ solve $ lines raw