import Data.List
import AOCUtil

parse :: [String] -> [[Int]]
parse [] = []
parse (row:xs) = (map (\c -> read [c]) row):(parse xs)

check :: [[Int]] -> [[Int]] -> (Int, Int) -> Bool
check g trg (x, y) = left || right || up || down
    where
        target = (g !! x) !! y
        left = all (<target) $ take y $ g !! x
        right = all (<target) $ drop (y + 1) $ g !! x
        up = all (<target) $ take x $ trg !! y
        down = all (<target) $ drop (x + 1) $ trg !! y

solve :: [String] -> Int
solve rawGrid = count (check parsed (transpose parsed)) [(r, c) | r <- [0..n-1], c <- [0..m-1]]
    where
        parsed = parse rawGrid
        n = length rawGrid
        m = length (head rawGrid)

main = do
    raw <- readFile "day8.in"
    print $ solve $ lines raw