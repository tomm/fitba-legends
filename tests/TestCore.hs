{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main where

import Control.Exception ( Exception, catch, throw, throwIO )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless, when)
import Database.Persist.Sqlite
import Prelude hiding ( catch )
import System.Directory (removeFile)
import System.Exit (exitFailure, exitSuccess)
import System.IO.Error (isDoesNotExistError)
import Control.Applicative ((<$>))
import Data.Maybe

import qualified DB
import Schema
import qualified Core
import qualified Settings
import qualified Types

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

dbTest m = do
    -- Keep a test.db file lying around at the end of tests, so that we
    -- can examine the DB in the case of failure
    removeFile "test.db" `catch` (\e -> unless (isDoesNotExistError e) (throwIO e))
    runSqlite "test.db" (runMigration migrateAll >> m)

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
        team <- insert $ Team "Test team" formation

        player1 <- insert $ Player team "Albert Einstein" 5
        player2 <- insert $ Player team "Kurt Schrödinger" 6
        player3 <- insert $ Player team "Charles Darwin" 4
        player4 <- insert $ Player team "Murray Gell-Mann" 8
        player5 <- insert $ Player team "Richard Feynman" 3
        player6 <- insert $ Player team "Johannes Kepler" 5
        player7 <- insert $ Player team "Julian Schwinger" 6
        player8 <- insert $ Player team "Marie Curie" 5
        player9 <- insert $ Player team "Isaac Newton" 8
        player10 <- insert $ Player team "Ludwig Boltzmann" 7
        player11 <- insert $ Player team "George Smoot" 4
        player12 <- insert $ Player team "Albert Michelson" 6
        player13 <- insert $ Player team "Edward Morley" 9

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
        league1 <- insert $ League "league 1" False
        league2 <- insert $ League "league 2" False

        -- hehe. I is learn haskell
        let makeTeam name = insert Formation >>= insert . Team name

        teamA <- makeTeam "Team A"
        teamB <- makeTeam "Team B"
        teamC <- makeTeam "Team C"
        teamD <- makeTeam "Team D"
        teamE <- makeTeam "Team E"
        teamF <- makeTeam "Team F"

        insert $ TeamLeague teamA league1
        insert $ TeamLeague teamB league1
        insert $ TeamLeague teamC league1
        insert $ TeamLeague teamD league1
        insert $ TeamLeague teamE league2
        insert $ TeamLeague teamF league2

        Core.makeFixtures league1

        -- no games played. check the league table is in team id order
        table <- Core.getLeagueTable league1
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

        table <- Core.getLeagueTable league1
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

        table <- Core.getLeagueTable league1
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
