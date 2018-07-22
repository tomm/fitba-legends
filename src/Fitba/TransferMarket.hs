module Fitba.TransferMarket where
import System.Random (RandomGen, newStdGen)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Random as Random
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Data.Time.Clock

import qualified Fitba.Player
import Fitba.Schema
import Fitba.DB (MonadDB, Con)
import qualified Fitba.Types as Types

decideTransferMarketBids :: IO ()
decideTransferMarketBids = pure ()

spawnNewTransferListings :: MonadDB a => [T.Text] -> Con a ()
spawnNewTransferListings surnamePool = do
  g <- liftIO System.Random.newStdGen
  let maybePlayer = fst $ Random.runRand (maybeRandomPlayer surnamePool) g
  case maybePlayer of
    Nothing -> pure ()
    Just player -> do
      now <- liftIO Data.Time.Clock.getCurrentTime
      key <- P.insert player
      P.insert $ TransferListing
        key
        (200000 * playerSkill player)
        (Data.Time.Clock.addUTCTime (60*60*24) now)
        Nothing
        Nothing
        Types.Active

      liftIO $ putStrLn $ "Transfer listing created: " ++ show player

maybeRandomPlayer :: (RandomGen g) => [T.Text] -> (Random.Rand g) (Maybe Player)
maybeRandomPlayer surnamePool = do
  dice <- Random.uniform [1..10]
  if dice == 1 then do
    maxSkill <- Random.uniform [4..9]
    Just <$> Fitba.Player.makeRandom 1 maxSkill surnamePool
  else
    pure Nothing
