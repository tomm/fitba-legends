{-# LANGUAGE OverloadedStrings #-}
module PopulateSchema where

import System.Random (newStdGen)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sqlite
import Data.Time.Clock as Clock
import Data.Time.Calendar as Calendar

import qualified DB (Con, MonadDB)
import Schema
import qualified Utils
import qualified Settings
import qualified Types

populate :: DB.MonadDB a => DB.Con a ()
populate = do
    league1 <- insert $ League "1st Division Season 1922"
    league2 <- insert $ League "2nd Division Season 1922"

    teamA <- insert $ Team "Team A"
    teamB <- insert $ Team "Team B"
    teamC <- insert $ Team "Team C"
    teamD <- insert $ Team "Team D"
    teamE <- insert $ Team "Team E"
    teamF <- insert $ Team "Team F"
    teamG <- insert $ Team "Team G"
    teamH <- insert $ Team "Team H"
    teamI <- insert $ Team "Team I"
    teamJ <- insert $ Team "Team J"
    teamK <- insert $ Team "Team K"
    teamL <- insert $ Team "Team L"
    teamM <- insert $ Team "Team M"
    teamN <- insert $ Team "Team N"

    insert $ TeamLeague teamA league1
    insert $ TeamLeague teamB league1
    insert $ TeamLeague teamC league1
    insert $ TeamLeague teamD league1
    insert $ TeamLeague teamE league1
    insert $ TeamLeague teamF league1
    insert $ TeamLeague teamG league1
    insert $ TeamLeague teamH league1
    insert $ TeamLeague teamI league1
    insert $ TeamLeague teamJ league1
    insert $ TeamLeague teamK league1
    insert $ TeamLeague teamL league1
    insert $ TeamLeague teamM league1
    insert $ TeamLeague teamN league1

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
