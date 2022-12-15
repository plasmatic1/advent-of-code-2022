import Data.Array (Array, listArray, (!), (//))
import Data.Char (ord)
import Data.List (findIndices, (!!))
import Data.Maybe (fromJust)
import Debug.Trace

type Grid = Array (Int, Int) Int

elevation :: Char -> Int
elevation 'S' = 0
elevation 'E' = 25
elevation c = ord c - ord 'a'

parse :: [String] -> Grid
parse grid = listArray ((0, 0), (length grid - 1, (length $ head grid) - 1)) $ concatMap (map elevation) grid

findInGrid :: [String] -> Char -> [(Int, Int)]
findInGrid grid goal = map (fst . (tagged !!)) $ findIndices (\(_, c) -> c == goal) tagged
    where
        n = length grid
        m = length $ head grid
        tagged = zip [(i, j) | i <- [0..n-1], j <- [0..m-1] ] $ concat grid

solve :: [String] -> Int
solve lines = let
    parsed = parse lines
    starts = findInGrid lines 'S' ++ findInGrid lines 'a'
    end = head $ findInGrid lines 'E'
    n = length lines
    m = length $ head lines

    inBound :: (Int, Int) -> Bool
    inBound (x, y) = 0 <= x && x < n && 0 <= y && y < m

    bfs :: Grid -> [(Int, Int)] -> Grid
    bfs g starts = helper starts $ (listArray ((0, 0), (n-1, m-1)) (repeat (-1))) // (zip starts (repeat 0))
        where
            helper :: [(Int, Int)] -> Grid -> Grid
            helper [] dist = dist
            helper (cur@(cx, cy):rest) dist = helper (rest ++ adj) (dist // [(to, (dist ! cur) + 1) | to <- adj])
                where adj = filter (\to -> inBound to && (g ! cur) + 1 >= g ! to && dist ! to == -1) [(cx + dx, cy + dy) | dx <- [-1..1], dy <- [-1..1], abs dx + abs dy == 1 ]
    in
        trace ("BFS grid == " ++ (show $ bfs parsed starts)) (bfs parsed starts) ! end


-- indices :: 

main = do
    lines <- readFile "day12.in" >>= (\r -> return $ lines r)
    print $ solve lines