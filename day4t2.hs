import AOCUtil
import Data.List

data Range = Range Int Int deriving Eq

rangeIntersection :: Range -> Range -> Range
rangeIntersection (Range a b) (Range c d) = (Range (max a c) (min b d))

rangeEmpty :: Range -> Bool
rangeEmpty (Range a b) = a > b

solve :: [Char] -> Bool
solve line = helper ((map read $ splitOn (\c -> c == ',' || c == '-') line) :: [Int])
    where
        helper :: [Int] -> Bool
        helper (a:b:c:d:[]) = not $ rangeEmpty $ rangeIntersection (Range a b) (Range c d)

main = do
    raw <- readFile "day4.in"
    print $ count solve $ lines raw