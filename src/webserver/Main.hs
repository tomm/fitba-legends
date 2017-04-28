{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
module Main where

import qualified Database.Persist as P
import Database.Persist.Sql
import Control.Monad.IO.Class (liftIO)
import Yesod
import Yesod.Static
import Database.Esqueleto as E
import GHC.Int (Int64)
import Data.Maybe
import Control.Arrow as Arrow
import Network.HTTP.Types (status201)

import qualified Fitba.DB as DB
import qualified Fitba.Types as Types
import qualified Fitba.Core as Core
import Fitba.Schema

data App = App { getStatic :: Static, getDbPool :: DB.ConnectionPool }

staticFiles "static"

mkYesod "App" [parseRoutes|
    /cmd CmdR GET
    /fixtures FixturesR GET
    /tables LeagueTablesR GET
    /load_game LoadGameR GET
    /squad/#Int64 SquadR GET
    /save_formation SaveFormationR POST
    /static StaticR Static getStatic
|]

instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        pool <- getDbPool <$> getYesod
        runSqlPool action pool

{-
import GHC.Generics (Generic)
data MyTurd = MyTurd { id :: Int, name :: Text } deriving (Show, Generic)
instance FromJSON MyTurd
-}

myTeamId :: TeamId
myTeamId = toSqlKey 1

postSaveFormationR :: HandlerT App IO ()
postSaveFormationR = do
    maybeTeam <- runDB $ get myTeamId
    case maybeTeam of
        Nothing -> notFound
        Just team -> do
            -- we don't check playerIds are ours, but Core.getPlayersOrdered will ignore non-owned players
            playerPoss <- requireJsonBody :: HandlerT App IO [(Int64, Maybe (Int, Int))]
            runDB $ Core.replaceFormationPositions (teamFormationId team) $ map (Arrow.first toSqlKey) playerPoss
            sendResponseStatus status201 ("SUCCESS" :: String)

getLoadGameR :: HandlerT App IO Yesod.Value
getLoadGameR = getSquadR 1

getSquadR :: Int64 -> HandlerT App IO Yesod.Value
getSquadR teamId = do
    maybeTeam <- runDB $ get (toSqlKey teamId :: TeamId)

    case maybeTeam of
        Nothing -> notFound
        Just team -> do
            playersAndFormation <- runDB $ Core.getPlayersOrdered (toSqlKey teamId) (teamFormationId team)

            return $ object ["id" .=  teamId,
                             "name" .= teamName team,
                             "players" .= map (playerToJson . fst) playersAndFormation,
                             "formation" .= mapMaybe snd playersAndFormation]
    where
        playerToJson p =
            object ["name" .= (playerName . entityVal) p,
                    "skill" .= (playerSkill . entityVal) p,
                    "id" .= (fromSqlKey . entityKey) p]

getLeagueTablesR :: HandlerT App IO Yesod.Value
getLeagueTablesR = do
    leagues <- runDB $ selectList [LeagueIsFinished P.==. False] []
    tables <- mapM (\l ->
            runDB $ Core.getLeagueTable (entityKey l)
        ) leagues
    return $ array $ map tableToJson (zip leagues tables)
    where
        tableToJson (league, table) =
            object ["name" .= (leagueName . entityVal) league,
                    "record" .= map teamRecToJson table]
        teamRecToJson (entityTeam, tournRec) =
            object ["teamId" .= (fromSqlKey . entityKey) entityTeam,
                    "name" .= (teamName . entityVal) entityTeam,
                    "played" .= Types.played tournRec,
                    "won" .= Types.won tournRec,
                    "drawn" .= Types.drawn tournRec,
                    "lost" .= Types.lost tournRec,
                    "goalsFor" .= Types.gf tournRec,
                    "goalsAgainst" .= Types.ga tournRec
                    ]

getFixturesR :: HandlerT App IO Yesod.Value
getFixturesR = do
    fixtures <- runDB $
        select $ from $ \(g, t1, t2) -> do
            where_ $
                (g ^. GameHomeTeamId E.==. t1 ^. TeamId) &&.
                (g ^. GameAwayTeamId E.==. t2 ^. TeamId)
            orderBy [asc (g ^. GameStart)]
            limit 10000
            return (g ^. GameId, t1 ^. TeamName, t2 ^. TeamName, g ^. GameStart, g ^. GameStatus)

    return $ array $ map resultToJsonObj fixtures
    where
        resultToJsonObj (gameId, team1Name, team2Name, start, status) =
            object ["gameId" .= unValue gameId,
                    "homeName" .= unValue team1Name,
                    "awayName" .= unValue team2Name,
                    "start" .= unValue start,
                    "status" .= unValue status]

getCmdR :: HandlerT App IO Yesod.Value
getCmdR = return $ object ["msg" .= ("Hello world" :: String)]

main :: IO ()
main = do
    static' <- static "static"

    DB.getPool "live.db" 4 $ \pool ->
        liftIO $ warp 3000 $ App static' pool
