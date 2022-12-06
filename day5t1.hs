import AOCUtil
import Data.Array
import Data.List
import Data.Char

type State = Array Int [Char]
data Move = Move Int Int Int

-- print the state
printAns :: State -> IO ()
printAns state = do
    helper $ elems state
    putChar '\n'
    where
        helper :: [[Char]] -> IO ()
        helper [] = return ()
        helper (stk:rest) = do
            putChar $ head stk
            helper rest

-- parses the initial configuration string into a state
parseState :: [String] -> State
parseState (_:[]) = listArray (1, 9) (replicate 9 []) -- ignore last line
parseState (x:xs) = parseLine prevState 1 x
    where prevState = parseState xs

parseLine :: State -> Int -> String -> State
parseLine curState idx (_:c:_:xs) = let newState = if c /= ' ' then curState // [(idx, c:(curState ! idx))] else curState
    in if null xs then newState else parseLine newState (idx + 1) (tail xs)

-- parses and applies moves
parseMove :: String -> Move
parseMove s = helper $ split ' ' s
    where helper (_:times:_:from:_:to:_) = Move (read times) (read from) (read to)

applyMove :: State -> Move -> State
applyMove state (Move 0 _ _) = state
applyMove state (Move n f t) = applyMove newState (Move (n - 1) f t)
    where fs = state ! f
          ts = state ! t
          newState = state // [(f, tail fs), (t, (head fs):ts)]

parseAndApplyMoves :: State -> [String] -> State
parseAndApplyMoves state [] = state
parseAndApplyMoves state (x:xs) = parseAndApplyMoves newState xs
    where newState = applyMove state $ parseMove x

main = do
    raw <- readFile "day5.in"
    let (initial:steps:[]) = split "" $ lines raw
        initialState = parseState initial
        finalState = parseAndApplyMoves initialState steps
        in do
            print initialState
            print finalState
            putStrLn "-- answer below --"
            printAns finalState