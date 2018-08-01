{-# LANGUAGE OverloadedStrings #-}
module DbTest where

import Control.Exception (catch, throwIO )
import Control.Monad.Logger (NoLoggingT, runStderrLoggingT, MonadLogger)
import Control.Monad.Reader (runReaderT)
import Control.Monad (unless, forM_)
import Data.Monoid
import Database.Persist.Postgresql
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

import Fitba.Schema
import qualified Fitba.Config
import qualified Fitba.DB as DB (Con, MonadDB)

--dbTest :: DB.MonadDB a => DB.Con a b -> IO b
dbTest action =
  Fitba.Config.load >>= \config ->
    --removeFile "test.db" `catch` (\e -> unless (isDoesNotExistError e) (throwIO e))
    runStderrLoggingT $ withPostgresqlConn (Fitba.Config.testDb config) $ \backend ->
      flip runReaderT backend $ do
        -- could I get a list of tables at runtime?
        forM_ ["user", "session", "league", "team", "team_league",
               "player", "formation", "formation_pos", "game",
               "game_event", "settings", "transfer_listing",
               "transfer_bid"] $ \tableName ->
          rawExecute ("DROP TABLE IF EXISTS \"" <> tableName <> "\" CASCADE") []
        runMigration migrateAll
        action
