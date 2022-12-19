import AOCUtil
import qualified Data.Set as Set
import Data.Set (Set, member)

type Coord = (Int, Int, Int)

parse :: String -> Coord
parse line = (a, b, c)
    where [a, b, c] = map read $ split ',' line :: [Int]

surfaceArea :: [Coord] -> Int
surfaceArea coords = (6 * length coords) - (sum $ map (\(x, y, z) -> length $ filter (`member` coordsSet) [(x+a, y+b, z+c) | a <- [-1..1], b <- [-1..1], c <- [-1..1], abs a + abs b + abs c == 1]) coords)
    where coordsSet = Set.fromList coords

main = do
    parsed <- readFile "day18.in" >>= (return . map parse . lines)
    print $ surfaceArea parsed