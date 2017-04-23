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
    isFinished Bool
    deriving Show

Team
    name Text
    formationId FormationId
    UniqueFormation formationId
    deriving Show Eq

TeamLeague
    teamId TeamId
    leagueId LeagueId
    UniqueTeamLeague teamId leagueId
    deriving Show

Player
    teamId TeamId
    name Text
    skill Int
    deriving Show

Formation
    deriving Show Eq

FormationPos
    formationId FormationId
    playerId PlayerId
    positionNum Int
    positionLoc Types.FormationPitchPos Maybe
    UniqueFormPos formationId positionNum
    UniqueFormPlayer formationId playerId
    deriving Show Eq

Game
    leagueId LeagueId
    homeTeamId TeamId
    awayTeamId TeamId
    status Types.GameStatus
    start UTCTime
    homeGoals Int default=0
    awayGoals Int default=0
    deriving Show

GameEvent
    gameId GameId
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
