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
import Data.ByteString
import Data.Time.Clock

import qualified Fitba.Types as Types
import qualified Fitba.TransferListing as TransferListing
import qualified Fitba.TransferBid as TransferBid

type PitchPos = (Int, Int)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    teamId TeamId
    secret ByteString
    UniqueUserName name
    UniqueTeamId teamId
    deriving Show

Session
    userId UserId
    identifier ByteString
    timestamp UTCTime
    UniqueSessionId identifier
    deriving Show

League
    name Text
    rank Int
    deriving Show

Team
    name Text
    formationId FormationId
    money Int
    deriving Show Eq

TeamLeague
    teamId TeamId
    leagueId LeagueId
    season Types.Season
    UniqueTeamLeagueSeason teamId leagueId season
    deriving Show

Player
    teamId TeamId Maybe
    name Text
    shooting Int
    passing Int
    tackling Int
    handling Int
    speed Int
    positions ByteString  -- json-encoded [(x,y)]
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
    season Types.Season
    deriving Show

GameEvent
    gameId GameId
    time UTCTime
    message Text Maybe
    ballPos PitchPos
    kind Types.GameEventType
    side Bool
    playerId PlayerId Maybe
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
    teamId TeamId Maybe -- seller
    status TransferListing.Status
    deriving Show

TransferBid
    teamId TeamId
    amount Int
    transferListingId TransferListingId
    status TransferBid.Status
    UniqueTeamBid teamId transferListingId
    deriving Show Eq
|]

playerSkill :: Player -> Int
playerSkill p = playerShooting p + playerPassing p + playerTackling p +
    playerHandling p + playerShooting p
