{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Settings where

import Control.Monad (void)
import Control.Monad.Catch (catch, throwM)
import Database.Persist.Sqlite
import Data.Text (Text)
import Database.Sqlite (seError, SqliteException, Error(ErrorConstraint))

import DB (Conn)
import Schema

-- toSqlKey :: Int -> Key record

get :: Text -> DB.Conn (Maybe Text)
get key = do
    kv <- getBy (UniqueKey key)
    case kv of
        Nothing -> return Nothing
        Just record -> (return . Just . settingsValue . entityVal) record

set :: Text -> Text -> DB.Conn ()
set key value =
    (void . insert) (Settings key value) `catch` \(e :: SqliteException) ->
        case seError e of
            ErrorConstraint -> updateWhere [SettingsKey ==. key] [SettingsValue =. value]
            _ -> throwM e
