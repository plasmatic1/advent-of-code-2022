import qualified Data.Set as Set
import AOCUtil

data Step = Up | Down | Left | Right deriving Show
type Coord = (Int, Int)

-- parsing
parse :: String -> [Step]
parse raw = flatMap parseOne $ lines raw
    where
        parseOne ('U':' ':x) = replicate (read x) Up
        parseOne ('D':' ':x) = replicate (read x) Down
        parseOne ('R':' ':x) = replicate (read x) Main.Right
        parseOne ('L':' ':x) = replicate (read x) Main.Left

-- apply steps
minimize :: (Ord n) => (a -> n) -> [a] -> a
minimize _ (x:[]) = x
minimize f (x:xs) = if f x < f best then x else best
    where best = minimize f xs

chebyshev :: Coord -> Coord -> Int
chebyshev (a,b) (c,d) = max (abs (a - c)) (abs (b - d))

manhattan :: Coord -> Coord -> Int
manhattan (a,b) (c,d) = (abs (a - c)) + (abs (b - d))

step :: Step -> Coord -> Coord
step Up (a, b) = (a, b + 1)
step Down (a, b) = (a, b - 1)
step Main.Right (a, b) = (a + 1, b)
step Main.Left (a, b) = (a - 1, b)

pullTowards :: Coord -> Coord -> Coord
pullTowards head tail@(tx, ty) = if chebyshev head tail <= 1
        then tail
        else minimize (manhattan head) [(tx + dx, ty + dy) | dx <- [-1..1], dy <- [-1..1]]

-- head is first element, etc.
applyList :: Step -> [Coord] -> [Coord]
applyList theStep (head:rest) = scanl pullTowards (step theStep head) rest

solve :: [Step] -> (Set.Set Coord, [Coord])
solve steps = foldl loop (Set.empty, replicate 10 (0, 0)) steps
    where
        loop :: (Set.Set Coord, [Coord]) -> Step -> (Set.Set Coord, [Coord])
        loop (tailLocs, snake) step = (Set.insert (last snake) tailLocs, newSnake)
            where newSnake = applyList step snake

main = do
    raw <- readFile "day9.in"
    let (resultSet, resultSnake) = solve $ parse raw
    print $ parse raw
    print resultSet
    print resultSnake
    putStrLn "--- ANSWER BELOW ---"
    print $ length resultSet