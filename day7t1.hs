import AOCUtil
import Data.Map hiding (split, map)

data Tree = Dir String (Map String Tree) | File String Int deriving Show

-- insert into a directory tree
insertTree :: Tree -> [String] -> Tree -> Tree
insertTree tree [] toInsert = let
    toInsertName = case toInsert of
        (Dir name _) -> name
        (File name _) -> name
        in case tree of
            (Dir name children) -> (Dir name (insert toInsertName toInsert children))

insertTree tree (x:xs) toInsert = case tree of
    (Dir name children) -> (Dir name (update (\child -> (Just (insertTree child xs toInsert))) x children))

-- parse input into directory tree
parseTree :: Tree -> [String] -> [String] -> Tree
parseTree state _ [] = state
parseTree state curPath (line:rest) = case split ' ' line of
    ["$", "cd", "/"] -> parseTree state [] rest
    ["$", "cd", ".."] -> parseTree state (init curPath) rest
    ["$", "cd", p] -> parseTree state (curPath ++ [p]) rest
    ["$", "ls"] -> parseTree state curPath rest
    ["dir", dir] -> parseTree (insertTree state curPath (Dir dir empty)) curPath rest
    [sz, file] -> parseTree (insertTree state curPath (File file (read sz))) curPath rest

-- actually solve problem, use recursive DFS
pairPlus :: (Int, Int) -> (Int, Int) -> (Int, Int)
pairPlus (a, b) (c, d) = (a + c, b + d)

solve :: Tree -> (Int, Int)
solve (File _ sz) = (0, sz)
solve (Dir _ children) = (if totChSz <= 100000 then totChAns + totChSz else totChAns, totChSz)
    where (totChAns, totChSz) = foldr1 pairPlus (map solve $ elems children)

main = do
    raw <- readFile "day7.in"
    let tree = parseTree (Dir "/" empty) [] (lines raw)
    print tree
    putStrLn "--- answer below ---"
    let (ans, _) = solve tree
    print ans