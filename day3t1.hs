import Data.Char
import Data.List

parse :: [Char] -> ([Char], [Char])
parse s = (take h s, drop h s)
    where h = (length s) `div` 2

priority :: Char -> Int
priority c
    | 'a' <= c && c <= 'z' = (ord c) - (ord 'a') + 1
    | otherwise = (ord c) - (ord 'A') + 27

solve :: ([Char], [Char]) -> Int
solve (a, b) = priority common
    where common = head $ intersect a b

main = do
    raw <- readFile "day3.in"
    let ans = sum $ map (solve . parse) $ lines raw
        in print ans