{-# LANGUAGE OverloadedStrings #-}
module Core where

import System.Random (newStdGen)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sqlite
import Data.Time.Clock as Clock
import Data.Time.Calendar as Calendar
import qualified Data.List (sortBy)

import qualified DB (Con, MonadDB)
import Schema
import qualified Utils
import qualified Settings
import qualified Types

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
    league1 <- insert $ League "1st Division Season 1922" False
    league2 <- insert $ League "2nd Division Season 1922" False

    teamA <- insert $ Team "Team A"
    teamB <- insert $ Team "Team B"
    teamC <- insert $ Team "Team C"
    teamD <- insert $ Team "Team D"
    teamE <- insert $ Team "Team E"
    teamF <- insert $ Team "Team F"
    teamG <- insert $ Team "Team G"
    teamH <- insert $ Team "Team H"

    insert $ TeamLeague teamA league1
    insert $ TeamLeague teamB league1
    insert $ TeamLeague teamC league1
    insert $ TeamLeague teamD league1
    insert $ TeamLeague teamE league2
    insert $ TeamLeague teamF league2
    insert $ TeamLeague teamG league2
    insert $ TeamLeague teamH league2

    makeFixtures league1

    return ()

getTeamsInLeague :: DB.MonadDB a => LeagueId -> DB.Con a [Entity Team]
getTeamsInLeague leagueId = do
    teamsLeague <- selectList [TeamLeagueLeagueId ==. leagueId] []
    selectList [TeamId <-. map (teamLeagueTeamId . entityVal) (teamsLeague :: [Entity TeamLeague])] []

makeFixtures :: DB.MonadDB a => LeagueId -> DB.Con a ()
makeFixtures leagueId = do
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
                    insert $ Game leagueId ((entityKey . fst) match) ((entityKey . snd) match) Types.Scheduled matchUTCTime 0 0
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