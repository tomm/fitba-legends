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
import Database.Persist.Sqlite (runSqlite)
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess)

import qualified Fitba.TransferMarket
import qualified Fitba.DB as DB

data DaemonData = DaemonData {
  surnamePool :: [T.Text],
  lastTick :: IORef (Maybe UTCTime)
  }

runDB = runSqlite "live.db"

simulateSeconds :: Integer -> IO ()
simulateSeconds seconds =
  replicateM_ (fromInteger seconds) $ do
    -- simulate games
    putStr "."

oncePerMinute :: DaemonData -> IO ()
oncePerMinute daemonData = do
  t <- getCurrentTime
  putStrLn $ "New minute! " ++ show t
  Fitba.TransferMarket.decideTransferMarketBids
  runDB $ Fitba.TransferMarket.spawnNewTransferListings (surnamePool daemonData)

oncePerDay :: UTCTime -> IO ()
oncePerDay t = do
    putStrLn "NEW DAY!!!!!!!!!!!!!"
    -- end of season resolving
    -- update team formations
    print t

callback :: DaemonData -> IO ()
callback daemonData = do
  timestamp <- getCurrentTime
  lastRun <- readIORef (lastTick daemonData)
  case lastRun of
    Nothing ->
      putStrLn "First run!"
    Just t -> do
      --putStrLn $ "Last run at " ++ show t
      simulateSeconds (secondsDiff timestamp t)
      if minutesDiff timestamp t > 0 then oncePerMinute daemonData else pure ()
      when (dayChanged t timestamp) $ oncePerDay timestamp

  writeIORef (lastTick daemonData) $ Just timestamp

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
  surnames <- T.splitOn "\n" <$> TIO.readFile "surname.txt"
  newIORef Nothing >>= \ref ->
    forever $ threadDelay 250000 >> callback (DaemonData surnames ref)
