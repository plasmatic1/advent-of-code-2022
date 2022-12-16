import AOCUtil
import Data.List (filter, sort, nub)

type Posn = (Int, Int)
type Seg = (Int, Int) -- it's an open closed range.  I.e. seg@(a, b) <=> [a, b)
type Report = (Posn, Posn) -- sensor, beacon

parse :: String -> Report
parse line = ((a, b), (c, d))
    where [a, b, c, d] = map read $ filter ((>0) . length) $ split ' ' $ filter (`elem` "-12345676890 ") $ line :: [Int]

manhattan :: Posn -> Posn -> Int
manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

row :: Int
row = 2000000

getSeg :: Report -> Seg
getSeg (sensor@(x, y), beacon) = let grow = manhattan sensor beacon - abs (y - row)
    in if grow < 0 then (0, 0) else (x - grow, x + grow + 1)

-- given a sortest list of segments, return the total number of units its union covers
unionLength :: [Seg] -> Int
unionLength xs' = let xs = filter (/=(0, 0)) xs'
    in helper minBound xs
        where
            helper :: Int -> [Seg] -> Int
            helper _ [] = 0
            helper lastR ((l, r):xs) = (max 0 (r - max l (lastR))) + helper (max lastR r) xs
        
main = do
    parsed <- readFile "day15.in" >>= (return . map parse . lines)
    putStrLn "--- signal/beacon locations ---"
    print parsed
    putStrLn "--- segments ---"
    let segments = sort $ map getSeg parsed
    print segments
    putStrLn "--- answer below ---" -- make sure to subtract out beacons on the bottom
    print $ unionLength segments - (length $ nub $ filter ((==row) . snd) $ map snd $ parsed)