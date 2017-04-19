{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Main where

import Database.Persist.Sql
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Yesod
import Yesod.Static
import Yesod.Persist.Core
import Database.Esqueleto as E

import qualified DB
import Schema

data App = App { getStatic :: Static, getDbPool :: DB.ConnectionPool }

staticFiles "static"

mkYesod "App" [parseRoutes|
    /cmd CmdR GET
    /fixtures FixturesR GET
    /static StaticR Static getStatic
|]

instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        pool <- getDbPool <$> getYesod
        runSqlPool action pool

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

main = do
    static' <- static "static"

    DB.getPool "live.db" 4 $ \pool ->
        liftIO $ warp 3000 $ App static' pool
