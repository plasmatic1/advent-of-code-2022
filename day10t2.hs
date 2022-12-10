import AOCUtil

data Inst = Noop | Addx Int

parse :: String -> [Inst]
parse raw = flatMap (helper . (split ' ')) $ lines raw
    where
        helper ["noop"] = [Noop]
        helper ["addx", x] = [Noop, Addx (read x)]

applyInst :: Int -> Inst -> Int
applyInst reg Noop = reg
applyInst reg (Addx x) = reg + x

solve :: [Inst] -> [[Char]]
solve insts = map (helperLine 0) $ chunksOf 40 regValues
    where
        regValues = scanl applyInst 1 insts

        helperLine :: Int -> [Int] -> [Char]
        helperLine _ [] = []
        helperLine pos (x:xs) = (if abs (pos - x) <= 1 then '#' else '.'):helperLine (pos + 1) xs

main = do
    raw <- readFile "day10.in"
    let parsed = parse raw
    let output = solve parsed
    mapM_ putStrLn output