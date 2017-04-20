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

main :: IO ()
main = do
    testCreateSchema
    testSettings
    testGetLeagueTable
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

assertEq msg a b = unless (a == b) (throw $ TestFailure $ "FAIL: " ++ msg ++ ": " ++ show a ++ " /= " ++ show b)
assertNe msg a b = unless (a /= b) (throw $ TestFailure $ "FAIL: " ++ msg ++ ": " ++ show a ++ " == " ++ show b)

testGetLeagueTable :: IOTest
testGetLeagueTable = do
    putStrLn "testGetLeagueTable..."
    dbTest $ do
        league1 <- insert $ League "league 1" False
        league2 <- insert $ League "league 2" False

        teamA <- insert $ Team "Team A"
        teamB <- insert $ Team "Team B"
        teamC <- insert $ Team "Team C"
        teamD <- insert $ Team "Team D"
        teamE <- insert $ Team "Team E"
        teamF <- insert $ Team "Team F"

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
