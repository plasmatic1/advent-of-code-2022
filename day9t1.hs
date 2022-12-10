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

step :: Step -> Coord -> Coord
step Up (a, b) = (a, b + 1)
step Down (a, b) = (a, b - 1)
step Main.Right (a, b) = (a + 1, b)
step Main.Left (a, b) = (a - 1, b)

applyOne :: Step -> (Coord, Coord) -> (Coord, Coord)
applyOne theStep (head, tail) = (nextHead, if chebyshev nextHead tail <= 1
        then tail
        else minimize (chebyshev tail) [(nhx+1, nhy), (nhx-1, nhy), (nhx, nhy+1), (nhx, nhy-1)])
    where nextHead@(nhx, nhy) = step theStep head

solve :: [Step] -> (Set.Set Coord, Coord, Coord)
solve steps = foldl loop (Set.empty, (0, 0), (0, 0)) steps
    where
        loop :: (Set.Set Coord, Coord, Coord) -> Step -> (Set.Set Coord, Coord, Coord)
        loop (tailLocs, head, tail) step = (Set.insert newTail tailLocs, newHead, newTail)
            where (newHead, newTail) = applyOne step (head, tail)

main = do
    raw <- readFile "day9.in"
    let (resultSet, resultHead, resultTail) = solve $ parse raw
    print $ parse raw
    print resultSet
    print resultHead
    print resultTail
    putStrLn "--- ANSWER BELOW ---"
    print $ length resultSet