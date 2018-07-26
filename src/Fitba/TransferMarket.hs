module Fitba.TransferMarket (
  decideTransferMarketBids, spawnNewTransferListings
  ) where
import System.Random (RandomGen, newStdGen)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import qualified Control.Monad.Random as Random
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Data.Time.Clock
import Data.Foldable (traverse_)

import qualified Fitba.Player
import Fitba.Schema
import Fitba.DB (MonadDB, Con)
import qualified Fitba.Types as Types
import qualified Fitba.Core as Core
import qualified Data.List

decideTransferMarketBids :: MonadDB a => Con a ()
decideTransferMarketBids = do
  now <- liftIO Data.Time.Clock.getCurrentTime
  expired <- P.selectList [TransferListingDeadline P.<. now,
                           TransferListingStatus P.==. Types.Active] []

  forM_ expired resolveTransferListing

resolveTransferListing :: MonadDB a => P.Entity TransferListing -> Con a ()
resolveTransferListing tl =
  P.get (transferListingPlayerId (P.entityVal tl)) >>= \maybePlayer -> case maybePlayer of
    Nothing -> liftIO $ putStrLn $ "Cannot resolve transfer bid. Player not found. " ++ show (P.entityVal tl)
    Just player -> resolveTransferListing' player

  where
    resolveTransferListing' :: MonadDB a => Player -> Con a ()
    resolveTransferListing' player = do
      bids <- P.selectList [TransferBidTransferListingId P.==. P.entityKey tl]
                           [P.Desc TransferBidAmount]
      filterM (canWin tl player) bids >>= \ws -> case ws of
        winner:_ -> do
          resolveWinner winner
          mapM_ resolveLoser $ Data.List.filter (/= winner) bids
          pure ()
        [] ->
          -- no winning bid
          mapM_ resolveLoser bids

    canWin :: MonadDB a
           => P.Entity TransferListing -> Player -> P.Entity TransferBid
           -> Con a Bool
    canWin tl' player bid' = do
      let bid = P.entityVal bid'
      maybeBuyerTeam <- P.get $ transferBidTeamId bid
      case maybeBuyerTeam of
        Nothing -> {- shouldn't happen -} pure False
        Just buyerTeam -> canWin' tl' player bid buyerTeam
      where
        canWin' tl' player bid buyerTeam = do
          starting11 <- getStarting11 (transferBidTeamId bid) (teamFormationId buyerTeam)
          if transferBidAmount bid >= (transferListingMinPrice . P.entityVal) tl' &&
             teamMoney buyerTeam >= transferBidAmount bid &&
             (fromIntegral . playerSkill) player <= averageSkill starting11 * 1.25
          then pure True else pure False

    -- XXX could avoid iterating over players twice by keeping count in fold
    averageSkill :: [Player] -> Float
    averageSkill players = let totSkill = foldr (\a b -> playerSkill a + b) 0 players
                           in fromIntegral totSkill / fromIntegral (length players) :: Float

    getStarting11 :: MonadDB a => TeamId -> FormationId -> Con a [Player]
    getStarting11 teamId formationId = map (P.entityVal . fst) <$> Core.getPlayersOrdered teamId formationId

    resolveWinner :: MonadDB a => P.Entity TransferBid -> Con a ()
    resolveWinner bid = pure ()

    resolveLoser :: MonadDB a => P.Entity TransferBid -> Con a ()
    resolveLoser bid = pure ()

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
