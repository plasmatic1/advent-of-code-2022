import AOCUtil
import Debug.Trace
import Data.List (findIndices)

data Packet = I Int | L [Packet] deriving (Eq, Show)

parseOne :: String -> Packet
parseOne ('[':xs) = (L (parseList $ init xs))
    where
        parseList :: String -> [Packet]
        parseList "" = []
        parseList xs = (parseOne (take fstLen xs)):(parseList (drop (fstLen + 1) xs))
            where fstLen = indexComma xs

        indexComma :: String -> Int
        indexComma s = helper s 0 0
            where
                helper [] _ i = i
                helper (',':_) 0 i = i
                helper ('[':xs) nesting i = helper xs (nesting + 1) (i + 1)
                helper (']':xs) nesting i = helper xs (nesting - 1) (i + 1)
                helper (x:xs) nesting i = helper xs nesting (i + 1)
parseOne v = (I (read v))
    
parse :: [[String]] -> [[Packet]]
parse = map (map parseOne)

check :: [Packet] -> Bool
check [(I x), (I y)] = x < y
check [xs@(L _), (I y)] = check [xs, (L [(I y)])]
check [(I x), ys@(L _)] = check [(L [(I x)]), ys]
check [(L []), (L (_:_))] = True
check [(L (_:_)), (L [])] = False
check [(L (x:xs)), (L (y:ys))]
    | x == y = check [(L xs), (L ys)]
    | otherwise = check [x, y]

main = do
    parsed <- readFile "day13.in" >>= (return . parse . split "" . lines)
    print parsed
    putStrLn "--- answer below ---"
    let indices = findIndices check parsed
    print $ sum indices + length indices -- convert to 1-indexed