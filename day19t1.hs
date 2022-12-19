import qualified Data.Map as Map
import Data.Map (Map, (!))
import Control.Monad.State.Lazy
import AOCUtil
import Data.List (elem)
import Debug.Trace

-- ID, ore cost, clay cost, obsidian costs (2), geode costs (2)
type Blueprint = (Int, Int, Int, Int, Int, Int, Int)

parse :: String -> Blueprint
parse line = (id, oreCost, clayCost, obsOreCost, obsClayCost, geoOreCost, geoObsCost)
    where [id, oreCost, clayCost, obsOreCost, obsClayCost, geoOreCost, geoObsCost] = map read $ filter (/="") $ split ' ' $ filter (`elem` "0123456789 ") line :: [Int]

-- compute DP state

-- # of non-geode resources, # of each robots
-- ore, clay, obsidian, ore robots, clay robots, obsidian robots, geode robots
type DPState = (Int, Int, Int, Int, Int, Int, Int)
type DPTable = Map DPState Int

doDP :: Blueprint -> DPTable
doDP (_, oreCost, clayCost, obsOreCost, obsClayCost, geoOreCost, geoObsCost) = execState (replicateM 24 advance) (Map.singleton (0, 0, 0, 1, 0, 0, 0) 0)
    where
        advance :: State DPTable ()
        advance = trace "start advance" $ do
            states <- get >>= (return . Map.assocs)
            put Map.empty
            mapM_ (\(state@(ore, clay, obs, oreR, clayR, obsR, geoR), geo) -> 
                let
                    ore' = ore + oreR
                    clay' = clay + clayR
                    obs' = obs + obsR
                    geo' = geo + geoR
                in do
                    update (ore', clay', obs', oreR, clayR, obsR, geoR) geo'
                    when (ore >= oreCost) (update (ore' - oreCost, clay', obs', oreR + 1, clayR, obsR, geoR) geo')
                    when (ore >= clayCost) (update (ore' - oreCost, clay', obs', oreR, clayR + 1, obsR, geoR) geo')
                    when (ore >= obsOreCost && clay >= obsClayCost) (update (ore' - obsOreCost, clay' - obsClayCost, obs', oreR, clayR, obsR + 1, geoR) geo')
                    when (ore >= geoOreCost && obs >= geoObsCost) (update (ore' - geoOreCost, clay', obs' - geoObsCost, oreR, clayR, obsR, geoR + 1) geo')
                ) states

        update :: DPState -> Int -> State DPTable ()
        update dpState@(_, _, _, oreR, clayR, obsR, _) val = state (\mp -> ((), if oreR <= 4 && clayR <= 4 && obsR <= 4 then Map.insertWith max dpState val mp else mp))

-- main
main = do
    bps <- readFile "day19.in" >>= (return . map parse . lines)
    putStrLn "--- blueprints ---"
    print bps
    putStrLn "--- DP answers ---"
    let anss = map (\((id, _, _, _, _, _, _), mp) -> (id, maximum $ Map.elems mp)) $ zip bps (map doDP bps)
    print anss
    putStrLn "--- answer below ---"
    print $ sum $ map (\(a, b) -> a * b) anss