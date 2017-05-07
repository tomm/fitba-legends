{-# LANGUAGE OverloadedStrings #-}
module DbTest where

import Control.Monad (unless)
import System.Directory (removeFile)
import Control.Exception (catch, throwIO )
import System.IO.Error (isDoesNotExistError)
import Database.Persist.Sqlite

import Fitba.Schema
import qualified Fitba.DB as DB (Con, MonadDB)

--dbTest :: DB.MonadDB a => DB.Con a b -> IO b
dbTest m = do
    -- Keep a test.db file lying around at the end of tests, so that we
    -- can examine the DB in the case of failure
    removeFile "test.db" `catch` (\e -> unless (isDoesNotExistError e) (throwIO e))
    runSqlite "test.db" (runMigration migrateAll >> m)
