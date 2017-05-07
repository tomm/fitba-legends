{-# LANGUAGE FlexibleContexts #-}
module Fitba.Match where

import qualified Data.Text as T
import qualified Control.Monad.Random as Random
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import Control.Monad.Trans.Class (lift)
import System.Random (RandomGen)
import Control.Monad.State as State
import Database.Persist.Sqlite
import qualified Data.List
import Control.Monad
import Text.Printf (printf)

import Fitba.Schema
import qualified Fitba.Core as Core
import qualified Fitba.Types as Types

type PlayerState = (PlayerId, Int, Int)
data Side = Home | Away deriving (Show, Eq)
--data BallState = Kickoff PlayerId | HeldBy PlayerId | PassingTo Side PlayerId | Free Int Int deriving (Show)
-- PlayPitchPos is ([0-4], [0-7]), while Types.FormationPitchPos is ([0..4], [1..5])
type PlayPitchPos = (Int, Int)

data GameState = GameState {
        home :: [PlayerState],
        away :: [PlayerState],
        homeGoals :: Int,
        awayGoals :: Int,
        msg :: Maybe T.Text,
        time :: Float, -- minutes
        ballPos :: PlayPitchPos
    }

instance Show GameState where
    show g =
        let showPitch players = concatMap showLine $ zip [0..] players
            showLine (y, line) = concatMap (showSquare y) (zip [0..] line) ++ "|\n"
            showSquare y (x, []) = ball x y ++ "     "
            showSquare y (x, [p]) = ball x y ++ printf " %02d  " (fromSqlKey p)
            showSquare y (x, [p, q]) = ball x y ++ printf "%02d,%02d" (fromSqlKey p) (fromSqlKey q)
            showSquare y (x, p:q:xs) = ball x y ++ printf " %dv%d " (length $ filter ((<12) . fromSqlKey) (p:q:xs))
                                                                     (length $ filter ((>11) . fromSqlKey) (p:q:xs))
            ball x y = if (x, y) == ballPos g then "|*" else "| "
        in "Pitch:\n" ++ (showPitch . playersOnTiles) (home g ++ away g)


playersOnTiles :: [PlayerState] -> [[[PlayerId]]]
playersOnTiles ps = do
    y <- [0..7]
    return $ do
        x <- [0..4]
        return $ map (\(pid,_,_) -> pid) $ filter (\(pid, px, py) -> x == px && y == py) ps

runRandLogger :: WriterT String (Random.Rand g) a -> g -> (a, String)
runRandLogger m g = fst $ Random.runRand (runWriterT m) g
                 -- snd is g'

formationPosition :: Bool -> Types.FormationPitchPos -> PlayPitchPos
formationPosition True (x, y) = (x, y+1)
formationPosition False  (x, y) = (4-x, 6-y)

kickoffPos :: Bool -> Types.FormationPitchPos -> PlayPitchPos
kickoffPos dir pos = let (x, y) = formationPosition dir pos
                         y' = if dir then min (y+1) 7 else max (y-1) 0 -- squeeze all team 1 tile into own side
                     in if dir then (x, max y' 4) else (x, min y' 3)  -- ensure attackers sit on own side

setupGame :: Side -> [(Entity Player, Maybe Types.FormationPitchPos)] -> [(Entity Player, Maybe Types.FormationPitchPos)] -> GameState
setupGame kickoffBy homePlayers awayPlayers =
    GameState
        (map (kickoffPlayerState Home False) $ zip [0..10] homePlayers)
        (map (kickoffPlayerState Away True) $ zip [0..10] awayPlayers)
        0 0
        Nothing
        0.0
        (formationPosition (kickoffBy == Away) (2, 3))
    where
        kickoffPlayerState :: Side -> Bool -> (Int, (Entity Player, Maybe Types.FormationPitchPos)) -> PlayerState
        kickoffPlayerState side playDirection (playerIdx, (ep, Just pos)) =
            if side == kickoffBy && playerIdx > 8 then
                -- first 2 attackers on kickoff side
                let (posX, posY) = formationPosition playDirection (2, 3)
                in (entityKey ep, posX, posY)
            else
                let (posX, posY) = kickoffPos playDirection pos
                in (entityKey ep, posX, posY)
        kickoffPlayerState _ _ (_, (_, Nothing)) = undefined

takeGameTurn :: (RandomGen g) => M.Map (Key Player) Player -> GameState -> WriterT String (Random.Rand g) GameState
takeGameTurn players state = do
    -- players take turns ordered by the tuple (1d(10+speed), positioning, random [0..1])
    playOrder' <- forM (home state ++ away state) $ \(playerId, _, _) -> do
        let player = players M.! playerId
        randomFactor <- Random.getRandom
        randomSpeed <- lift $ dice 1 (10 + playerSpeed player)
        return (playerId, (randomSpeed, playerPositioning player, randomFactor :: Float))

    let playOrder = fmap fst $ Data.List.sortBy playOrderSort playOrder'

    tell $ show playOrder

    State.execStateT (takePlayerTurn playOrder) state

    where
        playOrderSort :: Ord a => (Key Player, a) -> (Key Player, a) -> Ordering
        playOrderSort (_, a) (_, b) = compare b a

takePlayerTurn :: [Key Player] -> State.StateT GameState (WriterT String (Random.Rand g)) ()
takePlayerTurn playOrder =
    forM_ playOrder $ \playerId -> do
        state <- State.get
        State.put state

{-
game :: (RandomGen g) => WriterT String (Random.Rand g) ()
game = do
    tell "Hello dude\n"
    tell "(shooting, gk skill, distance)\n"
    forM_ (do { x <- [1,3..9]; y <- [1,3..9]; d <- [0..4]; return (x,y,d)}) $ \skills -> do
        tell $ show skills
        goals <- replicateM 100 (shoot skills)
        tell $ show $ sum (map (\x -> if x then 1 else 0) goals)
        tell "% success\n"
    return ()
-}

dice :: (RandomGen g) => Int -> Int -> (Random.Rand g) Int
dice n s = do
    rolls <- replicateM n (Random.getRandomR (1, s))
    return $ sum rolls

shoot :: (RandomGen g) => (Int, Int, Int) -> WriterT String (Random.Rand g) Bool
shoot (atkShooting, gkSkill, distance) = do
    a <- lift $ (+) <$> dice 1 40 <*> dice 6 atkShooting
    d <- lift $ (+) <$> dice 1 40 <*> dice 6 gkSkill
    return (a > d + 15*distance)
