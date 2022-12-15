import AOCUtil
import Debug.Trace
import Data.List (findIndex, sort)
import Data.Maybe (fromJust)

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

check :: [Packet] -> Ordering
check [(I x), (I y)]
    | x < y = LT
    | x == y = EQ
    | otherwise = GT

check [xs@(L _), (I y)] = check [xs, (L [(I y)])]
check [(I x), ys@(L _)] = check [(L [(I x)]), ys]
check [(L []), (L (_:_))] = LT
check [(L (_:_)), (L [])] = GT
check [(L (x:xs)), (L (y:ys))] = case check [x, y] of
    EQ -> check [(L xs), (L ys)]
    cmp -> cmp
check [(L []), (L [])] = EQ

instance Ord Packet where
    compare a b = check [a, b]

main = do
    parsed <- readFile "day13.in" >>= (return . parse . split "" . lines)
    print parsed
    putStrLn "--- answer below ---"
    let divider n = (L [(L [(I n)])])
    let sorted = sort ((divider 2):(divider 6):(concat parsed))
    let i1 = 1 + (fromJust $ findIndex (==(divider 2)) sorted)
    let i2 = 1 + (fromJust $ findIndex (==(divider 6)) sorted)
    print $ i1 * i2