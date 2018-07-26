{-# LANGUAGE TemplateHaskell            #-}
module Fitba.TransferBid where
import Database.Persist.TH

data Status = Pending | Won | OutBid | TeamRejected | PlayerRejected | InsufficientMoney deriving (Show, Read, Eq)
derivePersistField "Status"
