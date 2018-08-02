{-# LANGUAGE OverloadedStrings     #-}
module Fitba.Player where
import qualified Control.Monad.Random as Random
import System.Random (RandomGen)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson
import Data.Maybe

import Fitba.Schema
import qualified Fitba.Utils
import qualified Fitba.Types as Types

makeRandom :: (RandomGen g) => Int -> Int -> [T.Text] -> (Random.Rand g) Player
makeRandom skillMin skillMax surnamePool =
  Player <$> pure Nothing
         <*> randPlayerName surnamePool
         <*> Random.uniform [skillMin..skillMax]
         <*> Random.uniform [skillMin..skillMax]
         <*> Random.uniform [skillMin..skillMax]
         <*> Random.uniform [skillMin..skillMax]
         <*> Random.uniform [skillMin..skillMax]
         <*> pure ""
  >>= setPlayerPositions

setPlayerPositions :: (RandomGen g) => Player -> (Random.Rand g) Player
setPlayerPositions player = do
  pos <- pickPos player
  return (player { playerPositions = LBS.toStrict (Data.Aeson.encode pos) })
  where 
    pickPos :: (RandomGen g) => Player -> (Random.Rand g) (Maybe [[Int]])
    pickPos p =
      Fitba.Utils.fromFreqList $ [
        -- ST
        ([[1,1],[2,1],[3,1]], 3 * fromIntegral (playerShooting p)),
        -- AML
        ([[0,2],[0,1]], fromIntegral (playerShooting p + playerPassing p)),
        -- AMR
        ([[4,2],[1,1]], fromIntegral (playerShooting p + playerPassing p)),
        -- AMC
        ([[1,2],[2,2],[3,2]], 1.5 * fromIntegral (playerShooting p + playerPassing p)),
        -- ML
        ([[0,3],[0,2]], 2 * fromIntegral (playerPassing p)),
        -- MR
        ([[4,3],[4,2]], 2 * fromIntegral (playerPassing p)),
        -- MC
        ([[1,3],[2,3],[3,3]], 3 * fromIntegral (playerPassing p)),
        -- DML
        ([[0,4],[0,3]], fromIntegral (playerPassing p + playerTackling p)),
        -- DMR
        ([[4,4],[4,3]], fromIntegral (playerPassing p + playerTackling p)),
        -- DMC
        ([[1,4],[2,4],[3,4]], 1.5 * fromIntegral (playerPassing p + playerTackling p)),
        -- DL
        ([[0,5],[0,4]], 2 * fromIntegral (playerTackling p)),
        -- DR
        ([[4,5],[4,4]], 2 * fromIntegral (playerTackling p)),
        -- DC
        ([[1,5],[2,5],[3,5]], 3 * fromIntegral (playerTackling p)),
        -- G
        ([[2,6]], fromIntegral (playerHandling p))
        ]

randPlayerName :: (RandomGen g) => [T.Text] -> (Random.Rand g) T.Text
randPlayerName surnamePool = Random.uniform surnamePool

playerSkill :: Player -> Int
playerSkill p = playerShooting p + playerPassing p + playerTackling p +
    playerHandling p + playerShooting p

playerPositionsList :: Player -> [Types.FormationPitchPos]
playerPositionsList p =
  let pos :: BS.ByteString --T.Text
      pos = playerPositions p
  in  case Data.Aeson.decodeStrict pos of
      Nothing -> []
      Just ps -> ps
