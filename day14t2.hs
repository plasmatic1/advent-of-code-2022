import AOCUtil
import Data.Map (Map, (!), empty, insert, elems, assocs)
import qualified Data.Map as Map (lookup)
import Data.List ((!!))

data Cell = Air | Sand | Rock deriving (Show, Eq)
type Polyline = [(Int, Int)]
type Grid = Map (Int, Int) Cell

-- starting location of sand
start :: (Int, Int)
start = (500, 0)

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

-- finds lowest (max) y-coord of any rock in the grid
bottomOf :: Grid -> Int
bottomOf g = maximum $ map (snd . fst) $ filter ((==Rock) . snd) $ assocs g

simulate :: Grid -> Grid
simulate g = let
    bottom = bottomOf g + 2
    
    -- lookup but with explicit default value implemented
    fetch :: (Int, Int) -> Grid -> Cell
    fetch coord@(_, y) g 
        | y >= bottom = Rock
        | otherwise = case Map.lookup coord g of
            Nothing -> Air
            (Just c) -> c

    helper :: (Int, Int) -> Grid -> Grid
    helper sandLoc@(x, y) g
        | fetch (x, y) g == Sand = g -- hit bottom 
        | fetch (x, y + 1) g == Air = helper (x, y + 1) g
        | fetch (x - 1, y + 1) g == Air = helper (x - 1, y + 1) g
        | fetch (x + 1, y + 1) g == Air = helper (x + 1, y + 1) g
        | otherwise = helper start (insert sandLoc Sand g)

        in helper start g

main = do
    parsed <- readFile "day14.in" >>= (return . makeGrid . (map parse) . lines)
    let finalGrid = simulate parsed
    putStrLn "--- parsed polylines ---"
    print parsed
    putStrLn "--- final grid ---"
    print finalGrid
    putStrLn "--- ans below ---"
    print $ length $ filter (==Sand) $ elems finalGrid
