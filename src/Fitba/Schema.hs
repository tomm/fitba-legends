{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls             #-}
module Fitba.Schema where

import Database.Persist.TH
import Data.Text
import Data.Time.Clock

import qualified Fitba.Types as Types

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
