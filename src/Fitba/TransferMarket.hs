module Fitba.TransferMarket where
import System.Random (RandomGen, newStdGen)
import qualified Control.Monad.Random as Random
import qualified Data.Text as T

import qualified Fitba.Player
import Fitba.Schema

decideTransferMarketBids :: IO ()
decideTransferMarketBids = pure ()

spawnNewTransferListings :: [T.Text] -> IO ()
spawnNewTransferListings surnamePool = do
  g <- System.Random.newStdGen
  let maybePlayer = fst $ Random.runRand (maybeRandomPlayer surnamePool) g
  case maybePlayer of
    Nothing -> pure ()
    Just player -> do
      putStrLn "Would make player:"
      print player

maybeRandomPlayer :: (RandomGen g) => [T.Text] -> (Random.Rand g) (Maybe Player)
maybeRandomPlayer surnamePool = do
  dice <- Random.uniform [1..10]
  if dice == 1 then do
    maxSkill <- Random.uniform [4..9]
    Just <$> Fitba.Player.makeRandom 1 maxSkill surnamePool
  else
    pure Nothing
