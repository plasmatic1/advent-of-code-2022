import Data.Char
import Data.List
import AOCUtil

parse :: [Char] -> [[[Char]]]
parse raw = chunksOf 3 $ lines raw

priority :: Char -> Int
priority c
    | 'a' <= c && c <= 'z' = (ord c) - (ord 'a') + 1
    | otherwise = (ord c) - (ord 'A') + 27

solve :: [[Char]] -> Int
solve ss = priority c
    where c = head $ foldr1 intersect ss

main = do
    raw <- readFile "day3.in"
    let ans = sum $ map solve $ parse raw
        in print ans