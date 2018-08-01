{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Fitba.Settings where

import Control.Monad.Catch (catch, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)
import Database.Persist.Postgresql
import Data.Text (Text)

import Fitba.DB (MonadDB, Con)
import Fitba.Schema

-- toSqlKey :: Int -> Key record

get :: MonadDB a => Text -> Con a (Maybe Text)
get key = do
    kv <- getBy (UniqueKey key)
    case kv of
        Nothing -> return Nothing
        Just record -> (return . Just . settingsValue . entityVal) record

set :: MonadDB a => Text -> Text -> Con a ()
set key value =
    void $ upsert (Settings key value) [SettingsValue =. value]
    {- turns out I didn't need this crap. thank you upsert.
    (void . insert) (Settings key value) `catch` \(e :: SqliteException) ->
        case seError e of
            ErrorConstraint -> updateWhere [SettingsKey ==. key] [SettingsValue =. value]
            _ -> throwM e
        -}
