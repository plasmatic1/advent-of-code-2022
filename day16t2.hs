import AOCUtil
import Data.Map (Map, insert, insertWith, lookup, (!), empty)
import qualified Data.Map as Map
import Data.List (elem, groupBy)
import Control.Monad.State
import Data.Set (Set, difference, intersection)
import qualified Data.Set as Set
import Data.Foldable (foldlM)
import Debug.Trace

type Graph = (Map String [String], Map String Int)

-- parsing
parse :: String -> Graph
parse s = helper (empty, empty) $ lines s
    where
        helper :: Graph -> [String] -> Graph
        helper g [] = g
        helper (adj, flowRates) (line:xs) = let
            (_:from:flow:tos) = filter (/="") $ split ' ' $ filter (`elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ") line
            flowNum = read flow :: Int
            in
                helper (insertWith (++) from tos adj, if flowNum == 0 then flowRates else (insert from flowNum flowRates)) xs

-- DP??
type DPState = (String, Int, Set String)
type DPTable = Map DPState Int

getDPTable :: Graph -> DPTable
getDPTable (g, flowRates) = execState (iterate 0) $ Map.singleton ("AA", 0, Set.empty) 0
    where
        iterate :: Int -> State DPTable ()
        iterate 26 = return ()
        iterate n = do
            curStates <- get >>= (return . filter (\(_, t, _) -> t == n) . Map.keys)
            mapM_ (\state@(node, curTime, nodesOn) -> do {
                curVal <- get >>= (return . (! state));
                let altVal = curVal + (sum $ map (flowRates !) $ Set.elems nodesOn)
                in do {
                    updateTable (node, curTime + 1, if node `Map.member` flowRates then Set.insert node nodesOn else nodesOn) altVal;
                    mapM_ (\to -> updateTable (to, curTime + 1, nodesOn) altVal) (g ! node)
                };
            }) curStates

            iterate (n + 1)

        updateTable :: DPState -> Int -> State DPTable ()
        updateTable state newVal = do
            mp <- get
            put $ Map.insertWith max state newVal mp

solve :: Graph -> Int
solve graph@(g, flowRates) = let
    table_ = foldl (\mp ((_, _, nodesOn), ans) -> Map.insertWith max nodesOn ans mp) Map.empty $ filter (\((_, t, _), _) -> t == 26) $ Map.assocs $ getDPTable graph

    f :: Set String -> Int
    f nodesOn = case Map.lookup nodesOn table_ of
        Nothing -> 0
        (Just a) -> a

    in trace "computed DP states" $ maximum [ sv + tv | (s, sv) <- Map.assocs table_, (t, tv) <- Map.assocs table_, Set.null $ s `intersection` t ]

main = do
    graph@(a, f) <- readFile "day16.in" >>= (return . parse)
    putStrLn "--- adj list ---"
    print a
    putStrLn "--- flows ---"
    print f
    putStrLn "--- ans ---"
    print $ solve graph