{-# LANGUAGE TemplateHaskell            #-}
module Fitba.Types where

import Data.Aeson
import Data.Text as T

import Database.Persist.TH

type FormationPitchPos = (Int, Int)

-- for some reason persistent wants this imported in Schema.hs, not defined locally
data GameEventType = Boring | ShotTry | ShotMiss | ShotSaved | Goal | KickOff | EndOfGame deriving (Show, Read, Eq)
derivePersistField "GameEventType"

data GameStatus = Scheduled | InProgress | Played deriving (Show, Read, Eq)
derivePersistField "GameStatus"

instance ToJSON GameStatus where
    toJSON a = Data.Aeson.String $ T.pack $ show a

data TournRecord = TournRecord { points :: Int, played :: Int, won :: Int, drawn :: Int, lost :: Int, gf :: Int, ga :: Int, gd :: Int  } deriving (Show, Eq)

data TransferListingStatus = Active | Sold | Unsold deriving (Show, Read, Eq)
derivePersistField "TransferListingStatus"

instance Num TournRecord where
    a + b = TournRecord (points a + points b) (played a + played b) (won a + won b)
                        (drawn a + drawn b) (lost a + lost b) (gf a + gf b)
                        (ga a + ga b) (gd a + gd b)
    _ - _ = undefined
    _ * _ = undefined
    abs _ = undefined
    signum _ = undefined
    fromInteger _ = undefined

instance Ord TournRecord where
    compare a b
        | points a > points b = GT
        | points a < points b = LT
        | gd a > gd b = GT
        | gd a < gd b = LT
        | gf a > gf b = GT
        | gf a < gf b = LT
        | otherwise = EQ
