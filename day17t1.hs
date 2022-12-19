import AOCUtil
import qualified Data.Set as Set
import Data.Set (Set, intersection)
import Control.Monad.State.Lazy

type Posn = (Int, Int)
type Rock = Set Posn

-- base offset is from the bottom-left to make it easier to drop rocks
rocks :: [Rock]
rocks = cycle [
    {-
    ####
    -}
    Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]

    {-
     #
    ###
     #
    -}
    , Set.fromList [(0, 1), (1, 1), (2, 1), (1, 2), (1, 0)]

    {-
      #
      #
    ###
    -}
    , Set.fromList [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]

    {-
    #
    #
    #
    #
    -}
    , Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3)]

    {-
    ##
    ##
    -}
    , Set.fromList [(0, 0), (1, 0), (0, 1), (1, 1)] ]

parseJets :: String -> [Posn]
parseJets str = cycle $ map (\c -> case c of {'>' -> (1, 0); '<' -> (-1, 0)}) str

-- rocks, jets, set of taken positions
type BoardState = ([Rock], [Posn], Set Posn)

-- left/right borders == x=-1, x=7
-- bottom border == y=0

solve :: String -> Set Posn
solve str = finalPosns
    where
        (finalRocks, finalJets, finalPosns) = execState (step 2022) (rocks, (parseJets str), Set.empty)

        step :: Int -> State BoardState ()
        step 0 = return ()
        step n = do
            rock <- newRock
            dropRock rock
            step (n - 1)

        -- creates a new rock
        newRock :: State BoardState Rock
        newRock = do
            r <- unconsRocks
            locs <- getLocs

            let posx = 2
            let posy = (foldl max 0 $ map snd $ Set.elems locs) + 4
            return $ Set.map (&+(posx, posy)) r

        -- drops the rock to the bottom possible state, and then updates the board state
        dropRock :: Rock -> State BoardState ()
        dropRock rock = do
            j <- unconsJets

            canDrop <- ok (Set.map (&+j) rock)
            let rock' = if canDrop then Set.map (&+j) rock else rock
            let rock'' = (Set.map (&+(0, -1)) rock')
            canDrop' <- ok rock''
            if canDrop' then
                dropRock rock''
            else
                addLocs rock'

        -- returns whether a rock can be in that position
        ok :: Rock -> State BoardState Bool
        ok rock = do
            let xpos = map fst $ Set.elems rock
            let ypos = map snd $ Set.elems rock
            (_, _, locs) <- get

            return $ minimum xpos >= 0 && maximum xpos < 7 && minimum ypos > 0 && Set.null (rock `intersection` locs)

        unconsJets :: State BoardState Posn
        unconsJets = state (\(rocks, (j:jets), locs) -> (j, (rocks, jets, locs)))

        unconsRocks :: State BoardState Rock
        unconsRocks = state (\((r:rocks), jets, locs) -> (r, (rocks, jets, locs)))

        getLocs :: State BoardState (Set Posn)
        getLocs = state (\state@(_, _, locs) -> (locs, state))

        addLocs :: Set Posn -> State BoardState ()
        addLocs locs' = state (\(rocks, jets, locs) -> ((), (rocks, jets, foldl (\acc p -> Set.insert p acc) locs $ Set.elems locs')))

main = do
    raw <- readFile "day17.in"
    putStrLn "--- final posns ---"
    let posns = solve raw
    print posns
    putStrLn "--- answer below ---"
    print $ foldl max 0 $ map snd $ Set.elems posns