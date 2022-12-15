import AOCUtil
import Data.Map (Map, (!), empty, insert, elems)
import qualified Data.Map as Map (lookup)

data Cell = Air | Sand | Rock deriving (Show, Eq)
type Polyline = [(Int, Int)]
type Grid = Map (Int, Int) Cell

-- starting location of sand
start :: (Int, Int)
start = (500, 0)

-- lowest y-coord.  If sand reaches here we're done
bottom :: Int
bottom = 500

parse :: String -> Polyline
parse line = map (parseCoord . (split ',') . head) $ split "->" $ split ' ' line
    where parseCoord [x, y] = (read x, read y)

makeGrid :: [Polyline] -> Grid
makeGrid lines = helper lines empty
    where
        helper :: [Polyline] -> Grid -> Grid
        helper [] g = g
        helper ([c]:restLines) g = helper restLines (insert c Rock g)
        helper ((c1:c2:ys):restLines) g
            | c1 == c2 = helper ((c2:ys):restLines) g
            | otherwise = let delta = dir c1 c2
                in helper (((sum c1 delta):c2:ys):restLines) (insert c1 Rock g)

        sum :: (Int, Int) -> (Int, Int) -> (Int, Int)
        sum (a, b) (c, d) = (a + c, b + d)

        -- (dir from to) -> returns the direction of the movement from `from` to `to`
        dir :: (Int, Int) -> (Int, Int) -> (Int, Int)
        dir (a, b) (c, d)
            | a == c = (0, sign (d - b))
            | b == d = (sign (c - a), 0)
        
        sign :: Int -> Int
        sign x
            | x == 0 = 0
            | x > 0 = 1
            | otherwise = -1

simulate :: Grid -> Grid
simulate g = helper start g
    where
        helper :: (Int, Int) -> Grid -> Grid
        helper sandLoc@(x, y) g
            | y > bottom = g -- hit bottom 
            | fetch (x, y + 1) g == Air = helper (x, y + 1) g
            | fetch (x - 1, y + 1) g == Air = helper (x - 1, y + 1) g
            | fetch (x + 1, y + 1) g == Air = helper (x + 1, y + 1) g
            | otherwise = helper start (insert sandLoc Sand g)
        
        -- lookup but with explicit default value implemented
        fetch :: (Int, Int) -> Grid -> Cell
        fetch coord g = case Map.lookup coord g of
            Nothing -> Air
            (Just c) -> c

main = do
    parsed <- readFile "day14.in" >>= (return . makeGrid . (map parse) . lines)
    let finalGrid = simulate parsed
    putStrLn "--- parsed polylines ---"
    print parsed
    putStrLn "--- final grid ---"
    print finalGrid
    putStrLn "--- ans below ---"
    print $ length $ filter (==Sand) $ elems finalGrid
