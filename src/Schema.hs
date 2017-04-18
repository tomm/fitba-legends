{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Schema where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text
import Data.Time.Clock

import qualified Types

type PitchPos = (Int, Int)
type Season = Int

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
League
    name Text
    deriving Show

Team
    name Text
    league LeagueId
    --formation
    deriving Show

Player
    team TeamId
    name Text
    skill Int
    deriving Show

Game
    season Season
    homeTeam TeamId
    awayTeam TeamId
    status Types.GameStatus
    start UTCTime
    deriving Show

GameEvent
    game GameId
    type_ Types.GameEventType
    timestamp UTCTime
    message Text
    ballPos PitchPos
    deriving Show

Settings
    key Text
    value Text
    UniqueKey key
    deriving Show
|]
