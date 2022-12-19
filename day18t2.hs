import AOCUtil
import qualified Data.Set as Set
import Data.Set (Set, member, difference, union)
import Control.Monad.State.Lazy

type Coord = (Int, Int, Int)
type Vis = Set Coord

parse :: String -> Coord
parse line = (a, b, c)
    where [a, b, c] = map read $ split ',' line :: [Int]

manhattanAdj :: Coord -> [Coord]
manhattanAdj (x, y, z) = [(x+a, y+b, z+c) | a <- [-1..1], b <- [-1..1], c <- [-1..1], abs a + abs b + abs c == 1]

chebyshevAdj :: Coord -> [Coord]
chebyshevAdj (x, y, z) = [(x+a, y+b, z+c) | a <- [-1..1], b <- [-1..1], c <- [-1..1], a /= 0 || b /= 0 || c /= 0]

surfaceArea :: [Coord] -> Int
surfaceArea coords = (6 * length coords) - (sum $ map (\coord -> length $ filter (`member` coordsSet) $ manhattanAdj coord) coords)
    where coordsSet = Set.fromList coords

floodFill :: [Coord] -> (Coord -> Bool) -> (Coord -> [Coord]) -> Vis
floodFill starts ok adj = execState (mapM_ dfs starts) Set.empty
    where
        dfs :: Coord -> State Vis ()
        dfs cur = do
            if not (ok cur) then
                return ()
            else do
                vis <- get
                if cur `member` vis then
                    return ()
                else
                    (put $ Set.insert cur vis) >>
                    (mapM_ dfs $ adj cur)

outerSurfaceArea :: [Coord] -> Int
outerSurfaceArea cubes = surfaceArea $ Set.elems $ cubesSet `union` innerAir
    where
        innerAir = ((Set.fromList [(x, y, z) | x <- [llim..rlim], y <- [llim..rlim], z <- [llim..rlim]]) `difference` outerAir) `difference` outerCubes
        outerCubes = Set.filter (any (`member` outerAir) . manhattanAdj) cubesSet
        outerAir = floodFill [(llim, llim, llim)] (\c@(x, y, z) -> llim <= x && x <= rlim && llim <= y && y <= rlim && llim <= z && z <= rlim && (not $ c `member` cubesSet)) manhattanAdj
        cubesSet = Set.fromList cubes

        llim = -2
        rlim = 22

main = do
    parsed <- readFile "day18.in" >>= (return . map parse . lines)
    print $ outerSurfaceArea parsed