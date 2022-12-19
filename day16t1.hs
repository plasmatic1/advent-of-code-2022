import AOCUtil
import Data.Map (Map, insert, insertWith, lookup, (!), empty)
import qualified Data.Map as Map
import Data.List (elem)
import Control.Monad.State
import Data.Set (Set)
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

solve :: Graph -> Int
solve (g, flowRates) = evalState (recurse ("AA", 30, Set.empty)) empty
    where
        recurse :: DPState -> State DPTable Int
        recurse state@(node, timeLeft, nodesOn) = -- trace ("DP on state " ++ show state) $
            if timeLeft == 0 then
                return 0
            else do
                mp <- get
                case Map.lookup state mp of
                    (Just result) -> {-trace ("used precomputed result on " ++ show state) $-} return result
                    Nothing -> do
                        let add = sum $ map (flowRates!) $ Set.elems nodesOn
                        let childStates =
                                (if (not (Set.member node nodesOn)) && (Map.member node flowRates) then [(node, timeLeft - 1, Set.insert node nodesOn)] else []) ++
                                (node, timeLeft - 1, nodesOn) :
                                (map (\to -> (to, timeLeft - 1, nodesOn)) (g ! node))

                        let accum :: Int -> DPState -> State DPTable Int
                            accum curAns chState = do
                                chAns <- recurse chState
                                return $ max curAns (chAns + add)

                        res <- foldlM accum 0 childStates
                        mp' <- get
                        put $ insert state res mp'
                        return {-$ trace ("finish DP on state " ++ show state ++ " map state " ++ show mp') $-} res

main = do
    graph@(a, f) <- readFile "day16.in" >>= (return . parse)
    putStrLn "--- adj list ---"
    print a
    putStrLn "--- flows ---"
    print f
    putStrLn "--- ans ---"
    print $ solve graph