{-# LANGUAGE TemplateHaskell            #-}
module Fitba.TransferListing where
import Database.Persist.TH

data Status = Active | Sold | Unsold deriving (Show, Read, Eq)
derivePersistField "Status"

