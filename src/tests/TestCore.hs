{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main where

import Control.Exception ( Exception, catch, throw, throwIO )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless, when)
import Prelude hiding ( catch )
import Database.Persist.Postgresql
import System.Exit (exitFailure, exitSuccess)
import Control.Applicative ((<$>))
import Data.Maybe

import qualified Fitba.DB as DB
import Fitba.Schema
import qualified Fitba.Core as Core
import qualified Fitba.Settings as Settings
import qualified Fitba.Types as Types
import DbTest

type IOTest = IO ()

data TestFailure = TestFailure String deriving (Show)
instance Exception TestFailure

assertEq msg a b = unless (a == b) (throw $ TestFailure $ "FAIL: " ++ msg ++ ": " ++ show a ++ " /= " ++ show b)
assertNe msg a b = unless (a /= b) (throw $ TestFailure $ "FAIL: " ++ msg ++ ": " ++ show a ++ " == " ++ show b)

main :: IO ()
main = do
    testCreateSchema
    testSettings
    testGetLeagueTable
    testTeamFormationOrdering
    -- leave this one last, so test run leaves a populated test.db file
    testPopulateSchema

testCreateSchema :: IOTest
testCreateSchema = do
    putStrLn "Testing schema creation..."
    dbTest $ liftIO $ return ()

testSettings :: IOTest
testSettings =
    dbTest $ do
        Settings.get "season" >>= assertEq "Settings.get with no record" Nothing
        Settings.set "season" "1922"
        Settings.get "season" >>= assertEq "Settings.get new record" (Just "1922")
        Settings.set "season" "1923"
        Settings.get "season" >>= assertEq "Settings.get updated record" (Just "1923")

testTeamFormationOrdering :: IOTest
testTeamFormationOrdering =
    dbTest $ do
        formation <- insert Formation
        formation2 <- insert Formation -- just to check we don't load from here!
        team <- insert $ Team "Test team" formation 0

        let newPlayer team name = Player team name 5 5 5 5 5 "[[2,6]]"
        let jteam = Just team

        player1 <- insert $ newPlayer jteam "Albert Einstein"
        player2 <- insert $ newPlayer jteam "Kurt Schrödinger"
        player3 <- insert $ newPlayer jteam "Charles Darwin"
        player4 <- insert $ newPlayer jteam "Murray Gell-Mann"
        player5 <- insert $ newPlayer jteam "Richard Feynman"
        player6 <- insert $ newPlayer jteam "Johannes Kepler"
        player7 <- insert $ newPlayer jteam "Julian Schwinger"
        player8 <- insert $ newPlayer jteam "Marie Curie"
        player9 <- insert $ newPlayer jteam "Isaac Newton"
        player10 <- insert $ newPlayer jteam "Ludwig Boltzmann"
        player11 <- insert $ newPlayer jteam "George Smoot"
        player12 <- insert $ newPlayer jteam "Albert Michelson"
        player13 <- insert $ newPlayer jteam "Edward Morley"

        insert $ FormationPos formation player1 1 $ Just (1,2)
        insert $ FormationPos formation player2 8 $ Just (2,3)
        insert $ FormationPos formation player3 2 $ Just (3,4)
        insert $ FormationPos formation player4 7 Nothing
        insert $ FormationPos formation player5 3 $ Just (2,3)
        insert $ FormationPos formation player6 6 $ Just (2,3)
        insert $ FormationPos formation player7 4 $ Just (2,3)
        insert $ FormationPos formation player8 5 $ Just (2,3)
        insert $ FormationPos formation2 player1 1 $ Just (1,2)

        playerPositions <- Core.getPlayersOrdered team formation
        assertEq "getPlayersOrdered length" 13 (length playerPositions)
        assertEq "Player 1 name" "Albert Einstein" $ (playerName . entityVal . fst) (playerPositions!!0)
        assertEq "Player 1 pos" (Just (2,6)) $ snd (playerPositions!!0)  -- GK forced to (2,6) position regardless
        assertEq "Player 2 name" "Charles Darwin" $ (playerName . entityVal . fst) (playerPositions!!1)
        assertEq "Player 3 name" "Richard Feynman" $ (playerName . entityVal . fst) (playerPositions!!2)
        assertEq "Player 4 name" "Julian Schwinger" $ (playerName . entityVal . fst) (playerPositions!!3)
        assertEq "Player 5 name" "Marie Curie" $ (playerName . entityVal . fst) (playerPositions!!4)
        assertEq "Player 6 name" "Johannes Kepler" $ (playerName . entityVal . fst) (playerPositions!!5)
        assertEq "Player 7 name" "Murray Gell-Mann" $ (playerName . entityVal . fst) (playerPositions!!6)
        assertEq "Player 7 pos" (Just (1,3)) $ snd (playerPositions!!6)
        -- ^^ was set to Nothing, but we get a standard 442 position returned back (because Core hates empty positions)
        assertEq "Player 8 name" "Kurt Schrödinger" $ (playerName . entityVal . fst) (playerPositions!!7)
        assertEq "Player 8 pos" (Just (2,3)) $ snd (playerPositions!!7)
        assertEq "Player 9 name" "Isaac Newton" $ (playerName . entityVal . fst) (playerPositions!!8)
        assertEq "Player 10 name" "Ludwig Boltzmann" $ (playerName . entityVal . fst) (playerPositions!!9)
        assertEq "Player 11 name" "George Smoot" $ (playerName . entityVal . fst) (playerPositions!!10)
        assertEq "Player 12 name" "Albert Michelson" $ (playerName . entityVal . fst) (playerPositions!!11)
        assertEq "Player 13 name" "Edward Morley" $ (playerName . entityVal . fst) (playerPositions!!12)
        assertEq "Player 13 pos" Nothing $ snd (playerPositions!!12)

        return ()

testGetLeagueTable :: IOTest
testGetLeagueTable = do
    putStrLn "testGetLeagueTable..."
    dbTest $ do
        league1 <- insert $ League "league 1" 1
        league2 <- insert $ League "league 2" 1

        let makeTeam name = insert Formation >>= \f -> insert $ Team name f 0
            season = 1

        teamA <- makeTeam "Team A"
        teamB <- makeTeam "Team B"
        teamC <- makeTeam "Team C"
        teamD <- makeTeam "Team D"
        teamE <- makeTeam "Team E"
        teamF <- makeTeam "Team F"

        insert $ TeamLeague teamA league1 season
        insert $ TeamLeague teamB league1 season
        insert $ TeamLeague teamC league1 season
        insert $ TeamLeague teamD league1 season
        insert $ TeamLeague teamE league2 season
        insert $ TeamLeague teamF league2 season

        Core.makeFixtures league1 season

        -- no games played. check the league table is in team id order
        table <- Core.getLeagueTable league1 season
        assertEq "League table length" 4 (length table)
        assertEq "League table ordering" "Team A" $ (teamName . entityVal . fst) (table!!0)
        assertEq "League table ordering" "Team B" $ (teamName . entityVal . fst) (table!!1)
        assertEq "League table ordering" "Team C" $ (teamName . entityVal . fst) (table!!2)
        assertEq "League table ordering" "Team D" $ (teamName . entityVal . fst) (table!!3)
        assertEq "League table ordering" (Types.TournRecord 0 0 0 0 0 0 0 0) $ snd (table!!0)
        assertEq "League table ordering" (Types.TournRecord 0 0 0 0 0 0 0 0) $ snd (table!!1)
        assertEq "League table ordering" (Types.TournRecord 0 0 0 0 0 0 0 0) $ snd (table!!2)
        assertEq "League table ordering" (Types.TournRecord 0 0 0 0 0 0 0 0) $ snd (table!!3)

        game <- head <$> selectList [GameHomeTeamId ==. teamA, GameAwayTeamId ==. teamB] []
        update (entityKey game) [GameHomeGoals =. 1, GameAwayGoals =. 3, GameStatus =. Types.Played]

        table <- Core.getLeagueTable league1 season
        assertEq "League table length" 4 (length table)
        assertEq "League table ordering" "Team B" $ (teamName . entityVal . fst) (table!!0)
        assertEq "League table ordering" "Team C" $ (teamName . entityVal . fst) (table!!1)
        assertEq "League table ordering" "Team D" $ (teamName . entityVal . fst) (table!!2)
        assertEq "League table ordering" "Team A" $ (teamName . entityVal . fst) (table!!3)
        assertEq "League table ordering" (Types.TournRecord 3 1 1 0 0 3 1 2) $ snd (table!!0)
        assertEq "League table ordering" (Types.TournRecord 0 0 0 0 0 0 0 0) $ snd (table!!1)
        assertEq "League table ordering" (Types.TournRecord 0 0 0 0 0 0 0 0) $ snd (table!!2)
        assertEq "League table ordering" (Types.TournRecord 0 1 0 0 1 1 3 (-2)) $ snd (table!!3)

        game <- head <$> selectList [GameHomeTeamId ==. teamB, GameAwayTeamId ==. teamA] []
        update (entityKey game) [GameHomeGoals =. 2, GameAwayGoals =. 2, GameStatus =. Types.Played]

        table <- Core.getLeagueTable league1 season
        assertEq "League table length" 4 (length table)
        assertEq "League table ordering" "Team B" $ (teamName . entityVal . fst) (table!!0)
        assertEq "League table ordering" "Team A" $ (teamName . entityVal . fst) (table!!1)
        assertEq "League table ordering" "Team C" $ (teamName . entityVal . fst) (table!!2)
        assertEq "League table ordering" "Team D" $ (teamName . entityVal . fst) (table!!3)
        assertEq "League table ordering" (Types.TournRecord 4 2 1 1 0 5 3 2) $ snd (table!!0)
        assertEq "League table ordering" (Types.TournRecord 1 2 0 1 1 3 5 (-2)) $ snd (table!!1)
        assertEq "League table ordering" (Types.TournRecord 0 0 0 0 0 0 0 0) $ snd (table!!2)
        assertEq "League table ordering" (Types.TournRecord 0 0 0 0 0 0 0 0) $ snd (table!!3)
        --
testPopulateSchema :: IOTest
testPopulateSchema = do
    putStrLn "testPopulateSchema..."
    dbTest $ do
        Core.populateSchema
        --throw $ TestFailure "FUCK"
