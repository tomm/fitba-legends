{-# LANGUAGE OverloadedStrings #-}
module RandName where

import qualified Control.Monad.Random as Random
import Data.Monoid ((<>))
import Data.Text as T
import Data.Vector as V
import System.Random

forenames :: Vector Text
forenames = fromList [
    "Bob", "Amanda", "Joey", "Kate"
    ]

surnames :: Vector Text
surnames = fromList [
    "Morton", "Robertson", "Einstein", "Bettoni"
    ]

randName :: (RandomGen g) => Random.Rand g Text
randName = do
    i <- Random.getRandomR (0, V.length forenames - 1)
    j <- Random.getRandomR (0, V.length surnames - 1)
    return $ forenames ! i <> " " <> surnames ! j
