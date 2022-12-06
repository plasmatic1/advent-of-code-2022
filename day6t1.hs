import Data.List

findPos :: String -> Int -> Int
findPos s idx = if length (nub (take 4 s)) == 4 then idx else findPos (tail s) (idx + 1)

main = do
    raw <- readFile "day6.in"
    print $ findPos raw 4