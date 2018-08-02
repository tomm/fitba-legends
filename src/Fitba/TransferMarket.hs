module Fitba.TransferMarket (
  decideTransferMarketBids, spawnNewTransferListings
  ) where
import System.Random (RandomGen, newStdGen, randomR)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import qualified Control.Monad.Random as Random
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Data.Time.Clock
import Data.Foldable (traverse_)
import Data.IORef
import qualified Data.List

import qualified Fitba.Player
import Fitba.Schema
import Fitba.DB (MonadDB, Con)
import qualified Fitba.Types as Types
import qualified Fitba.TransferListing as TransferListing
import qualified Fitba.TransferBid as TransferBid
import qualified Fitba.Core as Core

decideTransferMarketBids :: MonadDB a => Con a ()
decideTransferMarketBids = do
  now <- liftIO Data.Time.Clock.getCurrentTime
  expired <- P.selectList [TransferListingDeadline P.<. now,
                           TransferListingStatus P.==. TransferListing.Active] []

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
      gotWinnerRef <- liftIO $ newIORef False

      forM_ bids $ \bid -> do
        gotWinner <- liftIO $ readIORef gotWinnerRef
        status <- resolveBidStatus tl player bid gotWinner
        P.update (P.entityKey bid) [TransferBidStatus P.=. status]
        if status == TransferBid.Won then do
          sellTo tl bid
          liftIO $ writeIORef gotWinnerRef True
        else
          pure ()

      gotWinner <- liftIO $ readIORef gotWinnerRef
      if not gotWinner then
        handleNoWinner tl
      else
        pure ()
        
    -- XXX bad. player auctions always succeed at any min price
    handleNoWinner :: MonadDB a => P.Entity TransferListing -> Con a ()
    handleNoWinner tl = 
      case transferListingTeamId (P.entityVal tl) of
        Nothing -> unsold
        Just sellerTeamId -> do
          maybeUser <- P.getBy $ UniqueTeamId sellerTeamId
          case maybeUser of
            Nothing -> unsold
            Just userEntity -> do
              P.update (P.entityKey tl) [TransferListingStatus P.=. TransferListing.Sold]
              P.update sellerTeamId [TeamMoney P.+=. bidMin]
              P.deleteWhere [FormationPosPlayerId P.==. playerId]
              P.update playerId [PlayerTeamId P.=. Nothing]
      where
        unsold = P.update (P.entityKey tl) [TransferListingStatus P.=. TransferListing.Unsold]
        playerId = (transferListingPlayerId . P.entityVal) tl
        bidMin = transferListingMinPrice $ P.entityVal tl

    
    sellTo :: MonadDB a => P.Entity TransferListing -> P.Entity TransferBid -> Con a ()
    sellTo tl bid = do
      case sellerTeamId of
        Nothing -> pure ()
        Just sellerTeamId' ->
          P.get sellerTeamId' >>= \maybeSeller -> case maybeSeller of
            Nothing -> pure ()
            Just seller -> do
              P.update sellerTeamId' [TeamMoney P.+=. bidAmount]
              P.deleteWhere [FormationPosPlayerId P.==. playerId]

      buyer' <- P.get buyerTeamId
      case buyer' of
        Nothing -> pure ()
        Just buyer -> do
          P.update buyerTeamId [TeamMoney P.-=. bidAmount]
          P.update playerId [PlayerTeamId P.=. Just buyerTeamId]

      P.update (P.entityKey tl)
        [TransferListingWinningBidId P.=. Just (P.entityKey bid),
         TransferListingStatus P.=. TransferListing.Sold]
      where
        sellerTeamId = transferListingTeamId $ P.entityVal tl
        buyerTeamId = transferBidTeamId $ P.entityVal bid
        bidAmount = (transferBidAmount . P.entityVal) bid
        playerId = (transferListingPlayerId . P.entityVal) tl


    resolveBidStatus :: MonadDB a
           => P.Entity TransferListing -> Player -> P.Entity TransferBid -> Bool
           -> Con a TransferBid.Status
    resolveBidStatus tl' player bid' gotWinner = do
      let bid = P.entityVal bid'
      maybeBuyerTeam <- P.get $ transferBidTeamId bid
      case maybeBuyerTeam of
        Nothing -> {- shouldn't happen -} pure TransferBid.Pending
        Just buyerTeam -> resolveBidStatus' tl' player bid buyerTeam
      where
        resolveBidStatus' tl' player bid buyerTeam = do
          starting11 <- getStarting11 (transferBidTeamId bid) (teamFormationId buyerTeam)
          if transferBidAmount bid < (transferListingMinPrice . P.entityVal) tl' then
            pure TransferBid.TeamRejected
          else if (fromIntegral . Fitba.Player.playerSkill) player > averageSkill starting11 * 1.25 then
            pure TransferBid.PlayerRejected
          else if teamMoney buyerTeam < transferBidAmount bid then
            pure TransferBid.InsufficientMoney
          else if gotWinner then
            pure TransferBid.OutBid
          else
            pure TransferBid.Won

    -- XXX could avoid iterating over players twice by keeping count in fold
    averageSkill :: [Player] -> Float
    averageSkill players = let totSkill = foldr (\a b -> Fitba.Player.playerSkill a + b) 0 players
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
  let (overValue, g') = System.Random.randomR (1.0 :: Float, 1.1 :: Float) g
      maybePlayer = fst $ Random.runRand (maybeRandomPlayer surnamePool) g'
  case maybePlayer of
    Nothing -> pure ()
    Just player -> do
      now <- liftIO Data.Time.Clock.getCurrentTime
      key <- P.insert player
      P.insert $ TransferListing
        key
        (truncate (overValue * 200000.0 * fromIntegral (Fitba.Player.playerSkill player)))
        (Data.Time.Clock.addUTCTime (60*60*24) now)
        Nothing
        Nothing
        TransferListing.Active

      liftIO $ putStrLn $ "Transfer listing created: " ++ show player

maybeRandomPlayer :: (RandomGen g) => [T.Text] -> (Random.Rand g) (Maybe Player)
maybeRandomPlayer surnamePool = do
  dice <- Random.uniform [1..10]
  if dice == 1 then do
    maxSkill <- Random.uniform [4..9]
    Just <$> Fitba.Player.makeRandom 1 maxSkill surnamePool
  else
    pure Nothing
