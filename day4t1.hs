import AOCUtil
import Data.List

data Range = Range Int Int deriving Eq

rangeUnion :: Range -> Range -> Range
rangeUnion (Range a b) (Range c d) = (Range (min a c) (max b d))

rangeEncloses :: Range -> Range -> Bool
rangeEncloses f s = u == f || u == s
    where u = rangeUnion f s

solve :: [Char] -> Bool
solve line = helper ((map read $ splitOn (\c -> c == ',' || c == '-') line) :: [Int])
    where
        helper :: [Int] -> Bool
        helper (a:b:c:d:[]) = rangeEncloses (Range a b) (Range c d)

main = do
    raw <- readFile "day4.in"
    print $ count solve $ lines raw