{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Data.IORef
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess)

import qualified Fitba.TransferMarket

data DaemonData = DaemonData { surnamePool :: [T.Text] }

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
  Fitba.TransferMarket.spawnNewTransferListings (surnamePool daemonData)

oncePerDay :: UTCTime -> IO ()
oncePerDay t = do
    putStrLn "NEW DAY!!!!!!!!!!!!!"
    -- end of season resolving
    -- update team formations
    print t

callback :: IORef (Maybe UTCTime) -> DaemonData -> IO ()
callback ref daemonData = do
    timestamp <- getCurrentTime
    lastRun <- readIORef ref
    case lastRun of
        Nothing -> do
            putStrLn "First run!"
        Just t -> do
            Fitba.TransferMarket.spawnNewTransferListings (surnamePool daemonData)
            --putStrLn $ "Last run at " ++ show t
            simulateSeconds (secondsDiff timestamp t)
            if minutesDiff timestamp t > 0 then oncePerMinute daemonData else pure ()
            when (dayChanged t timestamp) $ oncePerDay timestamp

    writeIORef ref $ Just timestamp

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
  print (take 5 surnames)
  newIORef Nothing >>= \ref ->
      forever $ threadDelay 250000 >> callback ref (DaemonData surnames)
