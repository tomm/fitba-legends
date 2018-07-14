{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Maybe
import Control.Monad
import Text.Printf (printf)
import Lens.Micro
import Lens.Micro.TH

import Fitba.Schema
import qualified Fitba.Core as Core
import qualified Fitba.Types as Types
import qualified Fitba.Utils as Utils

type PlayPitchPos = (Int, Int)
type PlayerState = (PlayerId, PlayPitchPos)
data Side = Home | Away deriving (Show, Eq)
-- PassedTo is a transient state that doesn't persist beyond one game turn
-- it indicates that the receiving player can't make another move after receiving
data BallState = Kickoff Side | HeldBy PlayerId | PassedTo PlayerId | Free deriving (Show)
-- PlayPitchPos is ([0-4], [0-7]), while Types.FormationPitchPos is ([0..4], [1..5])

data GameState = GameState {
        _players :: [PlayerState], -- 0-11 home, 12-21 away
        _homeGoals :: Int,
        _awayGoals :: Int,
        _msg :: Maybe T.Text,
        _time :: Float, -- minutes
        _ballPos :: PlayPitchPos,
        _ballState :: BallState
    }
makeLenses ''GameState

instance Show GameState where
    show g =
        let showPitch players = concatMap showLine $ zip [0..] players
            showLine (y, line) = concatMap (showSquare y) (zip [0..] line) ++ "|\n"
            showSquare y (x, []) = ball x y ++ "     "
            showSquare y (x, [p]) = ball x y ++ printf " %02d  " (fromSqlKey p)
            showSquare y (x, [p, q]) = ball x y ++ printf "%02d,%02d" (fromSqlKey p) (fromSqlKey q)
            showSquare y (x, p:q:xs) = ball x y ++ printf " %dv%d " (length $ filter ((<12) . fromSqlKey) (p:q:xs))
                                                                     (length $ filter ((>11) . fromSqlKey) (p:q:xs))
            ball x y = if (x, y) == _ballPos g then "|*" else "| "
        in "Ball: " ++ show (_ballState g) ++ ", pitch:\n" ++ (showPitch . playersOnTiles) (_players g)


playersOnTiles :: [PlayerState] -> [[[PlayerId]]]
playersOnTiles ps = do
    y <- [0..7]
    return $ do
        x <- [0..4]
        return $ map fst $ filter (\(pid, (px, py)) -> x == px && y == py) ps

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
        (map (kickoffPlayerState Home False) (zip [0..10] homePlayers) ++
         map (kickoffPlayerState Away True) (zip [0..10] awayPlayers))
        0 0
        Nothing
        0.0
        (formationPosition (kickoffBy == Away) (2, 3))
        (Kickoff kickoffBy)
    where
        kickoffPlayerState :: Side -> Bool -> (Int, (Entity Player, Maybe Types.FormationPitchPos)) -> PlayerState
        kickoffPlayerState side playDirection (playerIdx, (ep, Just pos)) =
            if side == kickoffBy && playerIdx > 8 then
                -- first 2 attackers on kickoff side
                let (posX, posY) = formationPosition playDirection (2, 3)
                in (entityKey ep, (posX, posY))
            else
                let (posX, posY) = kickoffPos playDirection pos
                in (entityKey ep, (posX, posY))
        kickoffPlayerState _ _ (_, (_, Nothing)) = undefined

takeGameTurn :: (RandomGen g) => M.Map (Key Player) Player -> GameState -> WriterT String (Random.Rand g) GameState
takeGameTurn players state = do
    return state
{-
    -- players take turns ordered by the tuple (1d(10+speed), positioning, random [0..1])
    playOrder' <- forM (zip [0..] (_players state)) $ \(playerIdx, (playerId, _)) -> do
        let player = players M.! playerId
        randomFactor <- Random.getRandom
        randomSpeed <- lift $ dice 1 (10 + playerSpeed player)
        return (playerIdx, (randomSpeed, playerPositioning player, randomFactor :: Float))

    let playOrder = fst <$> Data.List.sortBy playOrderSort playOrder'

    --tell $ "Play order: " ++ show playOrder

    State.execStateT (takePlayerTurns playOrder) state

    where
        playOrderSort :: Ord a => (Int, a) -> (Int, a) -> Ordering
        playOrderSort (_, a) (_, b) = compare b a
-}
data PlayerMove = Pass PlayerId | Move PlayPitchPos deriving (Show)

takePlayerTurns :: (RandomGen g) => [Int] -> State.StateT GameState (WriterT String (Random.Rand g)) ()
takePlayerTurns playOrder = do
    forM_ playOrder $ \playerIdx -> do
        state <- State.get
        let options = possibleMoves playerIdx state
        --lift $ tell $ show options
        chosen <- lift $ lift $ Utils.fromFreqList options
        --lift $ tell $ show chosen
        forM_ chosen $ \move -> performMove playerIdx move

    state <- State.get
    -- if a player was passed to then change ball state to HeldBy for next turn
    case _ballState state of
        HeldBy pid ->
            moveBallTo pid
        PassedTo pid -> do
            moveBallTo pid
            State.put (state & set ballState (HeldBy pid))
        _ -> return ()

moveBallTo :: (RandomGen g) => Key Player -> State.StateT GameState (WriterT String (Random.Rand g)) ()
moveBallTo playerId = do
    state <- State.get
    let (_, playerPos) = playerStateById playerId state
    State.put $ state & set ballPos playerPos

performMove :: (RandomGen g) => Int -> PlayerMove -> State.StateT GameState (WriterT String (Random.Rand g)) ()
performMove playerIdx move = do
    state <- State.get

    case moveType playerIdx state of
        OnBall -> lift $ tell $ "Player with ball does: " ++ show move ++ "\n"
        DoingKickoff -> lift $ tell $ "Kickoff! " ++ show move ++ "\n"
        _ -> return ()

    case move of
        Pass otherId ->
            State.put (state & set ballState (PassedTo otherId)
                             & set ballPos (snd $ playerStateById otherId state))
        Move toPos ->
            State.put (state & set (players . ix playerIdx . _2) toPos)

playerIdxSide :: Int -> Side
playerIdxSide idx
    | idx >= 0 && idx <= 10 = Home
    | idx >= 11 && idx <= 21 = Away
    | otherwise = undefined

pickSide :: Side -> GameState -> [PlayerState]
pickSide Home state = take 11 $ _players state
pickSide Away state = drop 11 $ _players state

pitchDist :: PlayPitchPos -> PlayPitchPos -> Double
pitchDist (x1, y1) (x2, y2) = sqrt (fromIntegral (x1-x2)**2 + fromIntegral (y1-y2)**2)

isOnPitch :: PlayPitchPos -> Bool
isOnPitch (x, y) = x >= 0 && x <= 4 && y >= 0 && y <= 7

addPos :: PlayPitchPos -> (Int, Int) -> PlayPitchPos
addPos (a,b) (c,d) = (a+c, b+d)

data PlayContext = OffBall | OnBall | JustReceived | DoingKickoff | WaitingKickoff

kickoffPlayerIdx :: Side -> Int
kickoffPlayerIdx Home = 10
kickoffPlayerIdx Away = 21

playerStateByIdx :: Int -> GameState -> PlayerState
playerStateByIdx playerIdx state = fromJust $ state ^? players . ix playerIdx

playerStateById :: Key Player -> GameState -> PlayerState
playerStateById playerId state = fromJust $ Data.List.find (\ps -> fst ps == playerId) (_players state)

moveType :: Int -> GameState -> PlayContext
moveType playerIdx state =
    case _ballState state of
        Kickoff side' -> if side == side' && kickoffPlayerIdx side == playerIdx then DoingKickoff else WaitingKickoff
                                             -- ^^ wrong if < 11 players XXX
        HeldBy pid -> if pid == playerId then OnBall else OffBall
        PassedTo pid -> if pid == playerId then JustReceived else OffBall
        Free -> OffBall
    where
        side = playerIdxSide playerIdx
        playerId = fst (playerStateByIdx playerIdx state)


possibleMoves :: Int -> GameState -> [(PlayerMove, Double)]
possibleMoves playerIdx state =
    case moveType playerIdx state of
        DoingKickoff -> possiblePasses
        WaitingKickoff -> []
        JustReceived -> []
        OffBall -> possibleMoves
        OnBall -> possibleMoves ++ possiblePasses
    where
        possibleMoves = catMaybes (fmap moveProbability (do { dx <- [-1..1]; dy <- [-1..1]; guard (isOnPitch (ourPos `addPos` (dx, dy))); return (dx, dy) }))
        possiblePasses = catMaybes (fmap passProbability (pickSide (playerIdxSide playerIdx) state))
        (playerId, ourPos) = playerStateByIdx playerIdx state
        passProbability :: PlayerState -> Maybe (PlayerMove, Double)
        passProbability (otherId, otherPos)
            | otherId == playerId = Nothing -- can't pass to ourself
            | otherwise = Just (Pass otherId, 1.0 / (1.0 + pitchDist otherPos ourPos))
        moveProbability :: (Int, Int) -> Maybe (PlayerMove, Double)
        moveProbability offset = Just (Move (ourPos `addPos` offset), 0.5)

{-
 if on ball
   can move or pass or shoot
 if off ball
   can move or receive pass or receive free ball or tackle
-}

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
