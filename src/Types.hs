{-# LANGUAGE TemplateHaskell            #-}
module Types where

import Database.Persist.TH

-- for some reason persistent wants this imported in Schema.hs, not defined locally
data GameEventType = Boring | HomeGoal | AwayGoal | EndOfGame deriving (Show, Read, Eq)
derivePersistField "GameEventType"

data GameStatus = Scheduled | Played deriving (Show, Read, Eq)
derivePersistField "GameStatus"
