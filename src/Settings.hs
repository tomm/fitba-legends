{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Settings where

import Control.Monad.Catch (catch, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)
import Database.Persist.Sqlite
import Database.Sqlite (seError, SqliteException, Error(ErrorConstraint))
import Data.Text (Text)

import DB (MonadDB, Con)
import Schema

-- toSqlKey :: Int -> Key record

get :: MonadDB a => Text -> Con a (Maybe Text)
get key = do
    kv <- getBy (UniqueKey key)
    case kv of
        Nothing -> return Nothing
        Just record -> (return . Just . settingsValue . entityVal) record

set :: MonadDB a => Text -> Text -> Con a ()
set key value =
    (void . insert) (Settings key value) `catch` \(e :: SqliteException) ->
        case seError e of
            ErrorConstraint -> updateWhere [SettingsKey ==. key] [SettingsValue =. value]
            _ -> throwM e
