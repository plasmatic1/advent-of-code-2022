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

indices :: Int -> [Int]
indices n = take n $ map (+20) $ map (*40) [0..]

solve :: [Inst] -> Int
solve insts = sum [i * (regValues !! (i - 1)) | i <- indices 6 ]
    where regValues = scanl applyInst 1 insts

main = do
    raw <- readFile "day10.in"
    let parsed = parse raw
    print $ solve parsed