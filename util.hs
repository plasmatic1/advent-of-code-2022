module AOCUtil where
import Data.List

splitOnHelper :: (a -> Bool) -> [a] -> [a] -> [[a]]
splitOnHelper f [] acc = [reverse acc]
splitOnHelper f (x:xs) acc = if f x
    then (reverse acc):(splitOnHelper f xs [])
    else splitOnHelper f xs (x:acc)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn f lst = splitOnHelper f lst []

split :: (Eq a) => a -> [a] -> [[a]]
split t lst = splitOn (== t) lst 

chunksOf :: Int -> [a] -> [[a]]
chunksOf sz [] = []
chunksOf sz xs = (take sz xs):(chunksOf sz (drop sz xs))

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs