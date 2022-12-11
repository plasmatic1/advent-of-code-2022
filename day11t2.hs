import AOCUtil
import qualified Data.List as List
import qualified Data.Array as Array
import Data.Array ((!), (//))
import Debug.Trace

data Val = Num Int | Old deriving Show
data Expr = Plus Val Val | Mul Val Val deriving Show
-- (items, updExpr, test, ifTrue, ifFalse)
type Monkey = ([Int], Expr, Int, Int, Int)
type Monkeys = Array.Array Int Monkey

-- parsing
parse :: String -> Monkeys
parse raw = Array.listArray (0, length lineGroups - 1) $ map (parseOne . (map (split ' '))) $ lineGroups  -- group lines and then split each line by spaces
    where
        lineGroups = split "" $ lines raw

        parseOne :: [[String]] -> Monkey
        parseOne [_, start, op, test, ifTrue, ifFalse] = (parseNumList (drop 4 start), parseExpr (drop 5 op), read (last test), read (last ifTrue), read (last ifFalse))

        parseNumList :: [String] -> [Int]
        parseNumList nums = map (read . (filter (/=','))) nums

        parseExpr :: [String] -> Expr
        parseExpr [x, "+", y] = (Plus (parseVal x) (parseVal y))
        parseExpr [x, "*", y] = (Mul (parseVal x) (parseVal y))

        parseVal :: String -> Val
        parseVal "old" = Old
        parseVal x = (Num (read x))

-- solving
monkeyPush :: Monkey -> Int -> Monkey
monkeyPush (lst, a, b, c, d) x = (lst ++ [x], a, b, c, d)

monkeyPoll :: Monkey -> Monkey
monkeyPoll (x:xs, a, b, c, d) = (xs, a, b, c, d)

-- cnt = array counting how many times each monkey has inspected an item
-- cnt, curMonkey, monkeyState
type State = (Array.Array Int Int, Monkeys)

wmod :: Int
wmod = 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19

solve :: String -> Int
solve raw = trace ("finalAnsArray = " ++ show finalAns) $ trace ("wmod = " ++ show wmod) (top1 * top2)
    where
        [top1, top2] = take 2 $ List.reverse $ List.sort $ Array.elems finalAns
        (finalAns, finalMonkeys) = foldl oneRound (Array.listArray (0, numMonkeys - 1) (replicate numMonkeys 0), parsed) (replicate 10000 ())

        oneRound :: State -> () -> State
        oneRound state _ = foldl oneMonkey state [0..numMonkeys - 1]

        oneMonkey :: State -> Int -> State
        oneMonkey state@(ans, monkeys) i = let monkey = monkeys ! i
            in case monkey of
                ([], _, _, _, _) -> state
                ((x:_), expr, test, ifTrue, ifFalse) -> let
                    newAns = ans // [(i, (ans ! i) + 1)]
                    newVal = evalExpr expr x
                    pushTo = if newVal `mod` test == 0 then ifTrue else ifFalse
                    newMonkeys = monkeys // [(i, monkeyPoll monkey), (pushTo, monkeyPush (monkeys ! pushTo) newVal)]
                    in
                        oneMonkey (newAns, newMonkeys) i
        
        evalExpr :: Expr -> Int -> Int
        evalExpr expr old = case expr of
                (Plus x y) -> (evalVal x + evalVal y) `mod` wmod
                (Mul x y) -> (evalVal x * evalVal y) `mod` wmod
            where
                evalVal Old = old
                evalVal (Num x) = x

        -- observation: all the test values are prime numbers from 2, ..., 19.  We can maintain divisibility properties even if we mod by the LCM
        -- haskell uses 64-bit int, so this is sufficient
        wmod = (foldl1 lcm $ map (\(_, _, test, _, _) -> test) $ Array.elems parsed)

        parsed = parse raw
        numMonkeys = length parsed

main = do
    raw <- readFile "day11.in"
    print $ Array.elems $ parse raw
    putStrLn "------ ANS BELOW ------"
    print $ solve raw