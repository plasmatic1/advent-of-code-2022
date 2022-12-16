import AOCUtil
import Data.List (filter, sort, nub, find)
import Data.Maybe (fromJust)

type Posn = (Int, Int)
type Seg = (Int, Int) -- it's an open closed range.  I.e. seg@(a, b) <=> [a, b)
type Report = (Posn, Posn) -- sensor, beacon

-- parsing
parse :: String -> Report
parse line = ((a, b), (c, d))
    where [a, b, c, d] = map read $ filter ((>0) . length) $ split ' ' $ filter (`elem` "-12345676890 ") $ line :: [Int]

-- figuring out which cells are OK
manhattan :: Posn -> Posn -> Int
manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

getSeg :: Int -> Report -> Seg
getSeg row (sensor@(x, y), beacon) = let grow = manhattan sensor beacon - abs (y - row)
    in if grow < 0 then (0, 0) else (x - grow, x + grow + 1)

-- given a sortest list of segments, return the total number of units its union covers
unionLength :: [Seg] -> Int -> Int -> Int
unionLength xs' lBound rBound = let xs = filter (/=(0, 0)) xs'
    in helper lBound xs
        where
            helper :: Int -> [Seg] -> Int
            helper _ [] = 0
            helper lastR ((l, r):xs) = (max 0 (min r (rBound + 1) - max l (lastR))) + helper (max lastR r) xs

numCanBeacon :: Int -> Int -> [Report] -> Int -> Int
numCanBeacon l r reports row = let
    segments = sort $ map (getSeg row) reports
    numCantBeacon = unionLength segments l r
    in (r - l + 1) - numCantBeacon

-- finding the correct square
findCanBeaconRow :: Int -> Int -> [Report] -> Int
findCanBeaconRow l r reports = fromJust $ find ((>0) . (numCanBeacon l r reports)) [i | i <- [l..r]]

findCanBeaconCol :: Int -> Int -> Int -> [Report] -> Int
findCanBeaconCol l r row reports = fromJust $ find check [i | i <- [l..r]]
    where
        check col = all (\(l, r) -> col < l || col >= r) segments
        segments = map (getSeg row) reports

coordL = 0
coordR = 4000000

main = do
    parsed <- readFile "day15.in" >>= (return . map parse . lines)
    putStrLn "--- signal/beacon locations ---"
    print parsed
    putStrLn "--- row ---"
    let row = findCanBeaconRow coordL coordR parsed
    print row
    putStrLn "--- col ---"
    let col = findCanBeaconCol coordL coordR row parsed
    print col
    putStrLn "--- ans below ---"
    print $ col * 4000000 + row