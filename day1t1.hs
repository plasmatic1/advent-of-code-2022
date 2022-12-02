module Main where

import AOCUtil
import Data.Typeable

asIntArrayAndSum :: [[[Char]]] -> [Int]
asIntArrayAndSum xs = map (\ys -> sum $ (map read ys :: [Int])) xs

main :: IO ()
main = do
    contents <- readFile "day1.in";
    let input = split "" $ lines contents
        inputAsInt = asIntArrayAndSum input
        in print $ maximum inputAsInt