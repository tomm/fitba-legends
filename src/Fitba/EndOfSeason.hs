module Fitba.EndOfSeason (
  handlePossibleEndOfSeason
  ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Database.Esqueleto as E
import Data.Map
import Data.Maybe
import Data.Time.Calendar (diffDays)
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Data.Time.Clock

import Fitba.DB (MonadDB, Con)
import Fitba.Types
import Fitba.Schema
import qualified Fitba.Core as Core

daysRestBetweenSeasons :: Integer
daysRestBetweenSeasons = 3
numTeamsPromoteRelegate :: Int
numTeamsPromoteRelegate = 3

handlePossibleEndOfSeason :: MonadDB a => Con a ()
handlePossibleEndOfSeason =
  isEndOfSeason >>= \isEnd ->
    when isEnd $ do
      today <- Data.Time.Clock.utctDay <$> liftIO Data.Time.Clock.getCurrentTime
      lastGameDay <- Data.Time.Clock.utctDay <$> dateOfLastGameThisSeason
      when (today `diffDays` lastGameDay >= daysRestBetweenSeasons) createNewSeason

createNewSeason :: MonadDB a => Con a ()
createNewSeason = do
  liftIO $ putStrLn "End of season!"
  last_season <- Core.currentSeason
  leagues <- P.selectList [] [P.Asc LeagueRank]
  let rankToLeagueId = Data.Map.fromList $ fmap (\l -> (leagueRank (P.entityVal l), P.entityKey l)) leagues
  -- make Team -> League associations for new season
  forM_ leagues $ \league -> do
    teamRank <- fmap fst <$> Core.getLeagueTable (P.entityKey league) last_season
    let promoteTo = fromMaybe (P.entityKey league) $
                      Data.Map.lookup (leagueRank (P.entityVal league) - 1) rankToLeagueId
        relegateTo = fromMaybe (P.entityKey league) $
                       Data.Map.lookup (leagueRank (P.entityVal league) + 1) rankToLeagueId
    addToLeague promoteTo (last_season + 1) $ take numTeamsPromoteRelegate teamRank
    addToLeague relegateTo (last_season + 1) $ take numTeamsPromoteRelegate $ reverse teamRank
    addToLeague (P.entityKey league) (last_season + 1) $
      drop numTeamsPromoteRelegate $ reverse $ drop numTeamsPromoteRelegate teamRank

  forM_ leagues $ \league -> do
    Core.makeFixtures (P.entityKey league) (last_season + 1)

  liftIO $ putStrLn "Finished processing end of season."

  where
    addToLeague :: MonadDB a => LeagueId -> Season -> [P.Entity Team] -> Con a ()
    addToLeague leagueId season teams =
      forM_ teams $ \team -> P.insert $ TeamLeague (P.entityKey team) leagueId season

isEndOfSeason :: MonadDB a => Con a Bool
isEndOfSeason = fmap (==0) numGamesLeftThisSeason

dateOfLastGameThisSeason :: MonadDB a => Con a Data.Time.Clock.UTCTime
dateOfLastGameThisSeason = do
  now <- liftIO Data.Time.Clock.getCurrentTime
  res <- select $ from $ \g -> do
    orderBy [desc (g ^. GameStart)]
    limit 1
    return (g ^. GameStart)
  case res of
    Value startTime : _ -> pure startTime
    [] -> pure now

numGamesLeftThisSeason :: MonadDB a => Con a Int
numGamesLeftThisSeason = do
  res <- select $ from $ \g -> do
    where_ (g ^. GameStatus !=. val Played)
    return (count (g ^. GameId))
  return $ unValue $ head res
