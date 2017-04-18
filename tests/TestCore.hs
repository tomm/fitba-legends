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

import DB (Conn)
import Schema
import qualified Settings

type IOTest = IO ()

data TestFailure = TestFailure String deriving (Show)
instance Exception TestFailure

main :: IO ()
main = do
    testCreateSchema
    testGetLeague
    testSettings

dbTest :: DB.Conn b -> IO b
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

assertEq msg b a = unless (a == b) (throw $ TestFailure $ "FAIL: " ++ msg ++ ": " ++ show a ++ " /= " ++ show b)
assertNe msg b a = unless (a /= b) (throw $ TestFailure $ "FAIL: " ++ msg ++ ": " ++ show a ++ " == " ++ show b)

testGetLeague :: IOTest
testGetLeague =
    dbTest $ do
        league1Id <- insert $ League "test league 1"
        league2Id <- insert $ League "test league 2"

        team1 <- insert $ Team "team 1" league1Id
        team2 <- insert $ Team "team 2" league1Id
        team3 <- insert $ Team "team 3" league1Id
        team4 <- insert $ Team "team 4" league2Id

        teams <- getLeagueTableOrdered league1Id

        liftIO $ print $ map teamName teams

        assertEq "getLeagueTableOrdered" (map teamName teams) ["team 1", "team 2", "team 3"]

getLeagueTableOrdered :: LeagueId -> DB.Conn [Team]
getLeagueTableOrdered leagueId = do
    teams <- selectList [TeamLeague ==. leagueId] []
    return $ map entityVal (teams :: [Entity Team])
