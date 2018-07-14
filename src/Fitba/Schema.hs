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
User
    name Text
    teamId TeamId
    secret Text
    money Int
    deriving Show

Session
    userId UserId
    identifier Text
    timestamp UTCTime
    deriving Show

League
    name Text
    rank Int
    deriving Show

Team
    name Text
    formationId FormationId
    deriving Show Eq

TeamLeague
    teamId TeamId
    leagueId LeagueId
    season Int
    UniqueTeamLeagueSeason teamId leagueId season
    deriving Show

Player
    teamId TeamId
    name Text
    shooting Int
    passing Int
    tackling Int
    handling Int
    speed Int
    positions Text  -- json-encoded [(x,y)]
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
    season Int
    deriving Show

GameEvent
    gameId GameId
    time UTCTime
    message Text
    ballPos PitchPos
    kind Types.GameEventType
    side Bool
    deriving Show

Settings
    key Text
    value Text
    UniqueKey key
    deriving Show

TransferListing
    playerId PlayerId
    minPrice Int
    deadline UTCTime
    winningBidId TransferBidId Maybe
    teamId TeamId -- seller
    status Types.TransferListingStatus
    deriving Show

TransferBid
    teamId TeamId
    amount Int
    transferListingId TransferListingId
    UniqueTeamBid teamId transferListingId
    deriving Show
|]
