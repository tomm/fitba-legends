{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Control.Monad.Logger (runStderrLoggingT, MonadLogger)
import Data.IORef
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Database.Persist.Postgresql
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess)

import qualified Fitba.EndOfSeason
import qualified Fitba.TransferMarket
import qualified Fitba.DB as DB
import qualified Fitba.Match
import qualified Fitba.Config

data DaemonData = DaemonData {
  surnamePool :: [T.Text],
  lastTick :: IORef (Maybe UTCTime)
  }

simulateSeconds :: DB.MonadDB a => DaemonData -> Integer -> DB.Con a ()
simulateSeconds daemonData seconds = Fitba.Match.simulatePendingGames

oncePerMinute :: DB.MonadDB a => DaemonData -> DB.Con a ()
oncePerMinute daemonData = do
  t <- liftIO getCurrentTime
  liftIO $ putStrLn $ "New minute! " ++ show t
  Fitba.TransferMarket.decideTransferMarketBids
  Fitba.TransferMarket.spawnNewTransferListings (surnamePool daemonData)

oncePerDay :: DB.MonadDB a => DaemonData -> UTCTime -> DB.Con a ()
oncePerDay daemonData t = do
    liftIO $ putStrLn "NEW DAY!!!!!!!!!!!!!"
    Fitba.EndOfSeason.handlePossibleEndOfSeason
    -- XXX update team formations

onStartup :: DB.MonadDB a => DaemonData -> DB.Con a ()
onStartup daemonData = do
  liftIO $ putStrLn "First run!"
  Fitba.EndOfSeason.handlePossibleEndOfSeason

loop :: DB.MonadDB a => DaemonData -> DB.Con a ()
loop daemonData =
  forever $ do
    timestamp <- liftIO getCurrentTime
    lastRun <- liftIO $ readIORef (lastTick daemonData)
    case lastRun of
      Nothing -> onStartup daemonData
      Just t -> do
        --putStrLn $ "Last run at " ++ show t
        simulateSeconds daemonData (secondsDiff timestamp t)
        if minutesDiff timestamp t > 0 then oncePerMinute daemonData else pure ()
        when (dayChanged t timestamp) $ oncePerDay daemonData timestamp

    liftIO $ writeIORef (lastTick daemonData) $ Just timestamp
    liftIO $ threadDelay 250000

  where
    secondsDiff :: UTCTime -> UTCTime -> Integer
    secondsDiff a b =
      ((diffTimeToPicoseconds . utctDayTime) a `quot` 1000000000000) -
      ((diffTimeToPicoseconds . utctDayTime) b `quot` 1000000000000)

    minutesDiff :: UTCTime -> UTCTime -> Integer
    minutesDiff a b =
      ((diffTimeToPicoseconds . utctDayTime) a `quot` 60000000000000) -
      ((diffTimeToPicoseconds . utctDayTime) b `quot` 60000000000000)

    dayChanged :: UTCTime -> UTCTime -> Bool
    dayChanged a b = utctDay a /= utctDay b

main :: IO ()
main = do
  config <- Fitba.Config.load
  surnames <- T.splitOn "\n" <$> TIO.readFile "surname.txt"
  newIORef Nothing >>= \ref ->
    DB.getPool (Fitba.Config.liveDb config) 1 $ \pool ->
      runSqlPool (loop (DaemonData surnames ref)) pool
