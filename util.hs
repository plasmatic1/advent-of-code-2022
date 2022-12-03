module AOCUtil where

splitHelper :: (Eq a) => a -> [a] -> [a] -> [[a]]
splitHelper t [] acc = [reverse acc]
splitHelper t (x:xs) acc = if x == t
    then (reverse acc):(splitHelper t xs [])
    else splitHelper t xs (x:acc)

split :: (Eq a) => a -> [a] -> [[a]]
split t lst = splitHelper t lst []

chunksOf :: Int -> [a] -> [[a]]
chunksOf sz [] = []
chunksOf sz xs = (take sz xs):(chunksOf sz (drop sz xs))