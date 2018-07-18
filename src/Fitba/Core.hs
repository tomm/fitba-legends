{-# LANGUAGE OverloadedStrings #-}
module Fitba.Core (
    getLeagueTable,
    makeFixtures,
    populateSchema,
    getPlayersOrdered,
    replaceFormationPositions
) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sqlite
import Data.Maybe
import Data.Time.Calendar as Calendar
import Data.Time.Clock as Clock
import qualified Control.Arrow as Arrow
import qualified Control.Monad.Random as Random
import qualified Database.Esqueleto as E
import qualified Data.List (sortBy)
import qualified Data.Text as T
import System.Random (RandomGen, newStdGen)

import Fitba.Schema
import qualified Fitba.DB as DB (Con, MonadDB)
import qualified Fitba.RandName as RandName
import qualified Fitba.Settings as Settings
import qualified Fitba.Types as Types
import qualified Fitba.Utils as Utils

formation442 = [
    (2, 6), -- gk
    (0, 5), (1, 5), (3, 5), (4, 5),
    (0, 3), (1, 3), (3, 3), (4, 3),
    (1, 1), (3, 1) 
    ]

-- ordered GK first, then 10 on pitch, reserves, etc
getPlayersOrdered :: DB.MonadDB a => TeamId -> FormationId -> DB.Con a [(Entity Player, Maybe Types.FormationPitchPos)]
getPlayersOrdered teamId formationId = do
    results <- E.select $
        E.from $ \(p `E.LeftOuterJoin` fp) -> do
            E.on ((E.just (p E.^. PlayerId) E.==. fp E.?. FormationPosPlayerId) E.&&.
                  ((fp E.?. FormationPosFormationId) E.==. E.just (E.val formationId)))
            E.where_ (p E.^. PlayerTeamId E.==. E.just (E.val teamId))
            E.orderBy [E.asc (E.isNothing $ fp E.?. FormationPosId), E.asc (fp E.?. FormationPosPositionNum)]
            return (p, fp E.?. FormationPosPositionLoc)

    return $ map munge $ zip [0..] results
    where
        -- always give a default position to players 0-10
        munge (idx, (p, pos))
            | idx == 0 = (p, (Just . head) formation442)  -- GK must remain in place
            | idx < 11 = (p, Just (fromMaybe (formation442 !! idx) ((Control.Monad.join . E.unValue) pos)))
                                   --                           ^^ Maybe (Maybe a) -> Maybe a
            | otherwise = (p, Nothing)


-- ordered descending (ie !!0 is in first place)
getLeagueTable :: DB.MonadDB a => LeagueId -> DB.Con a [(Entity Team, Types.TournRecord)]
getLeagueTable leagueId = do
    -- should replace this all with a big esqueleto query
    teams <- getTeamsInLeague leagueId
    points <- mapM (calcTeamPointsAndGoalDiff leagueId) teams

    return $ Data.List.sortBy leagueSort (zip teams points)

leagueSort :: (Entity Team, Types.TournRecord) -> (Entity Team, Types.TournRecord) -> Ordering
leagueSort (teamA, recA) (teamB, recB) = compare recB recA

calcTeamPointsAndGoalDiff :: DB.MonadDB a => LeagueId -> Entity Team -> DB.Con a Types.TournRecord
calcTeamPointsAndGoalDiff leagueId team = do
    games <- selectList ( [GameLeagueId ==.  leagueId, GameStatus ==. Types.Played] ++
          ([GameHomeTeamId ==. entityKey team] ||. [GameAwayTeamId ==. entityKey team]) ) []
    
    return $ foldr ((+) . pointsNGd . entityVal) (Types.TournRecord 0 0 0 0 0 0 0 0) (games :: [Entity Game])

    where
        pointsNGd' ourGoals theirGoals
            | ourGoals > theirGoals = 
                Types.TournRecord 3 1 1 0 0 ourGoals theirGoals (ourGoals - theirGoals)
            | ourGoals < theirGoals = 
                Types.TournRecord 0 1 0 0 1 ourGoals theirGoals (ourGoals - theirGoals)
            | otherwise = 
                Types.TournRecord 1 1 0 1 0 ourGoals theirGoals (ourGoals - theirGoals)

        pointsNGd :: Game -> Types.TournRecord
        pointsNGd game =
            if gameHomeTeamId game == entityKey team then
                pointsNGd' (gameHomeGoals game) (gameAwayGoals game)
            else
                pointsNGd' (gameAwayGoals game) (gameHomeGoals game)

populateSchema :: DB.MonadDB a => DB.Con a ()
populateSchema = do
    league1 <- insert $ League "1st Division Season 1922" 1
    league2 <- insert $ League "2nd Division Season 1922" 2

    teamA <- makeTeam "Team A"
    teamB <- makeTeam "Team B"
    teamC <- makeTeam "Team C"
    teamD <- makeTeam "Team D"
    teamE <- makeTeam "Team E"
    teamF <- makeTeam "Team F"
    teamG <- makeTeam "Team G"
    teamH <- makeTeam "Team H"

    insert $ TeamLeague teamA league1 1
    insert $ TeamLeague teamB league1 1
    insert $ TeamLeague teamC league1 1
    insert $ TeamLeague teamD league1 1
    insert $ TeamLeague teamE league2 1
    insert $ TeamLeague teamF league2 1
    insert $ TeamLeague teamG league2 1
    insert $ TeamLeague teamH league2 1

    makeFixtures league1 1

    return ()

    where
        makeTeam :: DB.MonadDB a => T.Text -> DB.Con a (Key Team)
        makeTeam name = do
            formation <- insert Formation
            team <- insert $ Team name formation
            g <- liftIO newStdGen
            players <- mapM insert $ fst $ Random.runRand (replicateM 22 (makeRandomPlayer team)) g

            replaceFormationPositions formation $ zip players (map Just formation442)

            return team

makeRandomPlayer :: (RandomGen g) => TeamId -> Random.Rand g Player
makeRandomPlayer teamId = do
    name <- RandName.randName
    shooting <- Random.getRandomR (1,9)
    passing <- Random.getRandomR (1,9)
    tackling <- Random.getRandomR (1,9)
    handling <- Random.getRandomR (1,9)
    speed <- Random.getRandomR (1,9)
    return $ Player (Just teamId) name shooting passing tackling handling speed "[[2,6]]"

replaceFormationPositions :: DB.MonadDB a => FormationId -> [(PlayerId, Maybe Types.FormationPitchPos)] -> DB.Con a ()
replaceFormationPositions formationId plId_pitchPos = do
    deleteWhere [FormationPosFormationId ==. formationId]
    mapM_ (\(idx, (playerId, loc)) -> insert $ FormationPos formationId playerId idx loc)
        $ zip [1..] plId_pitchPos

getTeamsInLeague :: DB.MonadDB a => LeagueId -> DB.Con a [Entity Team]
getTeamsInLeague leagueId = do
    teamsLeague <- selectList [TeamLeagueLeagueId ==. leagueId] []
    selectList [TeamId <-. map (teamLeagueTeamId . entityVal) (teamsLeague :: [Entity TeamLeague])] []

makeFixtures :: DB.MonadDB a => LeagueId -> Int -> DB.Con a ()
makeFixtures leagueId seasonNum = do
    teams <- getTeamsInLeague leagueId
    --liftIO $ print $ map entityVal (teams :: [Entity Team])
    let allMatches = do
            teamA <- teams
            teamB <- teams
            guard (teamA /= teamB)
            return (teamA, teamB)

    g <- liftIO newStdGen
    let (allMatchesShuffled, _) = Utils.shuffle g allMatches

    -- start new season fixtures tomorrow
    startDate <- liftIO $ fmap (addDays 1 . Clock.utctDay) Clock.getCurrentTime

    mapM_ (makeDayOfFixtures startDate) $ zip [0..] $ scheduleDays allMatchesShuffled

    where
        makeDayOfFixtures :: DB.MonadDB a => Calendar.Day -> (Int, [(Entity Team, Entity Team)]) -> DB.Con a ()
        makeDayOfFixtures startDate (dayNum, matches) = do
            let startTime = 10*60
                secondsBetweenGames = 12*60 `quot` max 1 (length matches - 1)
                matchDay = addDays (toInteger dayNum) startDate
            liftIO $ putStrLn ("Day " ++ show dayNum ++ ":")
            liftIO $ putStrLn ("Will happen on " ++ show matchDay)
            mapM_ (\(matchNum, match) -> do
                    let secondsAfterMidnight = Clock.secondsToDiffTime $ toInteger $ 60 * (startTime + secondsBetweenGames*matchNum)
                        matchUTCTime = Clock.UTCTime matchDay secondsAfterMidnight
                    insert $ Game leagueId ((entityKey . fst) match) ((entityKey . snd) match) Types.Scheduled matchUTCTime 0 0 seasonNum
                ) (zip [0..] matches)

-- Schedule lists of pairs into 'days', where no 'a' appears twice in a given day
scheduleDays :: Eq a => [(a, a)] -> [[(a, a)]]
scheduleDays [] = []
scheduleDays candidates =
    let hasDupTeam :: Eq a => (a, a) -> [(a, a)] -> Bool
        hasDupTeam match [] = False
        hasDupTeam match (x:xs) = fst match == fst x || fst match == snd x || 
                                  snd match == fst x || snd match == snd x ||
                                  hasDupTeam match xs

        pickNonConflicting :: Eq a => [(a, a)] -> [(a, a)] -> ([(a, a)], [(a, a)])
        pickNonConflicting chosen [] = (chosen, [])
        pickNonConflicting chosen (candidate:candidates) =
            if candidate `hasDupTeam` chosen then
                let (chosen', remaining') = pickNonConflicting chosen candidates
                in (chosen', candidate : remaining')
            else
                pickNonConflicting (candidate : chosen) candidates

        (day, candidates') = pickNonConflicting [] candidates

    in day : scheduleDays candidates'
