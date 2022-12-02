module Main where

import AOCUtil
import Data.Typeable
import Data.List

asIntArrayAndSum :: [[[Char]]] -> [Int]
asIntArrayAndSum xs = map (\ys -> sum $ (map read ys :: [Int])) xs

main :: IO ()
main = do
    contents <- readFile "day1.in";
    let input = split "" $ lines contents
        inputAsInt = asIntArrayAndSum input
        in print $ sum (take 3 $ reverse $ sort inputAsInt)