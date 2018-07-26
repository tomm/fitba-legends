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
import Data.Monoid
import Control.Monad
import Control.Arrow as Arrow
import Network.HTTP.Types (status201)
import qualified Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Time.Clock
import qualified Data.Map
import qualified Data.Text.Encoding

import qualified Fitba.DB as DB
import qualified Fitba.Types as Types
import qualified Fitba.Core as Core
import qualified Fitba.Hash
import Fitba.Schema

data App = App { getStatic :: Static, getDbPool :: DB.ConnectionPool }

staticFiles "static"

mkYesod "App" [parseRoutes|
    / IndexR GET
    /login LoginR GET
    /try_login TryLoginR POST
    /fixtures FixturesR GET
    /tables LeagueTablesR GET
    /load_world LoadGameR GET
    /squad/#TeamId SquadR GET
    /save_formation SaveFormationR POST
    /static StaticR Static getStatic
    /transfer_listings TransferListingsR GET
    /game_events/#GameId GameR GET
    /game_events_since/#GameId GameEventsAllR GET
    /game_events_since/#GameId/#GameEventId GameEventsSinceR GET
    /sell_player SellPlayerR POST
    /transfer_bid TransferBidR POST
|]

instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        pool <- getDbPool <$> getYesod
        runSqlPool action pool

getIndexR :: HandlerT App IO Html
getIndexR =
    getUser >>= \user -> case user of
        Nothing -> redirect LoginR
        Just _ -> index
    where
        index = defaultLayout $ do
            addStylesheet $ StaticR style_css
            toWidget [hamlet|
                <div id=main>
                <script src="/static/main.js">
                <script>
                    var node = document.getElementById('main');
                    var app = Elm.Main.embed(node);
            |]

getLoginR :: HandlerT App IO Html
getLoginR = defaultLayout $ do
    addStylesheet $ StaticR style_css
    toWidget [whamlet|
        <h1>Fitba Login
        <div class="login">
            <form method=post action=@{TryLoginR}>
                <label for=name>Username:
                <input type=text name=name>
                <br>
                <label for=password>Password:
                <input type=password name=secret>
                <br>
                <input type=submit>
    |]

postTryLoginR :: HandlerT App IO Html
postTryLoginR = do
    user <- lookupPostParam "name"
    secret <- lookupPostParam "secret"
    case (,) <$> user <*> secret of
        Nothing -> do
            liftIO $ putStrLn "invalid input"
            redirect LoginR
        Just (user, secret) -> tryLogin user (Data.Text.Encoding.encodeUtf8 secret)

    where
        tryLogin :: T.Text -> BS.ByteString -> HandlerT App IO Html
        tryLogin user secret = do
            user <- runDB $ getBy $ UniqueUserName user
            case ((== Fitba.Hash.md5sum secret) . userSecret . entityVal) <$> user of
                Nothing -> do
                    liftIO $ putStrLn "invalid user"
                    redirect LoginR
                Just False -> do
                    liftIO $ putStrLn "invalid secret"
                    redirect LoginR
                Just True -> do
                    now <- liftIO Data.Time.Clock.getCurrentTime
                    let sessionHash = Fitba.Hash.md5sum (secret <> (Data.Text.Encoding.encodeUtf8 . T.pack . show) now)
                    runDB $ insert $ Session (entityKey $ fromJust user) sessionHash now
                    setSession "session" (Data.Text.Encoding.decodeUtf8 sessionHash)
                    redirect IndexR

postSaveFormationR :: HandlerT App IO ()
postSaveFormationR =
    authGuard $ \user ->
        runDB (get (userTeamId user)) >>= saveTeamFormation
    where
        saveTeamFormation :: Maybe Team -> HandlerT App IO ()
        saveTeamFormation Nothing = notFound
        saveTeamFormation (Just team) = do
            -- we don't check playerIds are ours, but Core.getPlayersOrdered will ignore non-owned players
            playerPoss <- requireJsonBody :: HandlerT App IO [(Int64, Maybe (Int, Int))]
            runDB $ Core.replaceFormationPositions (teamFormationId team) $ map (Arrow.first toSqlKey) playerPoss
            sendResponseStatus status201 ("SUCCESS" :: String)

data PostTransferBid = PostTransferBid {
        postTransferBidListingId :: Int64, postTransferBidAmount :: Maybe Int
    } deriving (Show)
instance FromJSON PostTransferBid where
    parseJSON (Object o) = PostTransferBid
        <$> o .: "transfer_listing_id"
        <*> o .: "amount"
    parseJSON _ = mempty

getUser :: HandlerT App IO (Maybe User)
getUser = do
    sessionMap <- getSession
    case Data.Map.lookup "session" sessionMap of
        Nothing -> pure Nothing
        Just sessionId ->
            (runDB . getBy . UniqueSessionId) sessionId
            >>= \sess -> case sess of
                Nothing -> pure Nothing
                Just sess' -> getUserForSession (entityVal sess')
    where
        getUserForSession :: Session -> HandlerT App IO (Maybe User)
        getUserForSession sess = runDB $ get (sessionUserId sess)

postTransferBidR :: HandlerT App IO Yesod.Value
postTransferBidR =
    authGuard $ \user -> do
        bid <- requireJsonBody :: HandlerT App IO PostTransferBid
        let teamId = userTeamId user
            tlId = toSqlKey (postTransferBidListingId bid) :: TransferListingId
        maybeTl <- runDB $ get tlId
        now <- liftIO Data.Time.Clock.getCurrentTime
        case maybeTl of
            Nothing -> notFound
            Just tl ->
                if transferListingDeadline tl < now then
                    return $ object ["status" .= "EXPIRED"]
                else
                    case postTransferBidAmount bid of
                        Nothing -> do
                            -- erase bid
                            runDB $ E.delete $ from $ \b ->
                                where_ $
                                    (b ^. TransferBidTeamId E.==. E.val (userTeamId user)) &&.
                                    (b ^. TransferBidTransferListingId E.==. E.val tlId)
                            return $ object ["status" .= "SUCCESS"]
                        Just amount -> do
                            your_bid <- runDB $ getBy (UniqueTeamBid teamId tlId)
                            case your_bid of
                                Nothing -> void $ runDB $ insert $ TransferBid teamId amount tlId
                                Just bid -> void $ runDB $ E.update $ \b -> do
                                    set b [ TransferBidAmount E.=. E.val amount ]
                                    where_ (b ^. TransferBidId E.==. E.val (entityKey bid))
                            return $ object ["status" .= "SUCCESS"]

data PostSellPlayer = PostSellPlayer { postSellPlayerId :: Int64 } deriving (Show)
instance FromJSON PostSellPlayer where
    parseJSON (Object o) = PostSellPlayer
        <$> o .: "player_id"
    parseJSON _ = mempty

postSellPlayerR :: HandlerT App IO Yesod.Value
postSellPlayerR =
    authGuard $ \user -> do
        let teamId = userTeamId user
        sellPlayer <- requireJsonBody :: HandlerT App IO PostSellPlayer
        let playerId = toSqlKey (postSellPlayerId sellPlayer) :: PlayerId
        player' <- runDB $ select $ from $ \p -> do
            where_ $ (p ^. PlayerId E.==. E.val playerId) &&.
                     (p ^. PlayerTeamId E.==. E.just (E.val teamId))
            return p
        case player' of
            [player] -> do
                now <- liftIO Data.Time.Clock.getCurrentTime
                -- XXX I just want COUNT(*)!!!!
                active_listings <- runDB $ select $ from $ \tl -> do
                    where_ $
                        (tl ^. TransferListingPlayerId E.==. E.val playerId) &&.
                        (tl ^. TransferListingStatus E.==. E.val Types.Active)
                    return (tl ^. TransferListingId)
                if null active_listings then do
                    runDB $ insert (TransferListing
                        playerId
                        (200000 * playerSkill (entityVal player))
                        (Data.Time.Clock.addUTCTime (60*60*24) now)
                        Nothing
                        (Just teamId)
                        Types.Active)
                    return $ object ["status" .= "SUCCESS"]
                else
                    return $ object ["status" .= "SUCCESS"]
            _ -> notFound


getLoadGameR :: HandlerT App IO Yesod.Value
getLoadGameR =
    authGuard $ \user -> getSquadR (userTeamId user)

getSquadR :: TeamId -> HandlerT App IO Yesod.Value
getSquadR teamId =
    authGuard $ \user ->
        runDB (get teamId) >>= \maybeTeam ->
            case maybeTeam of
                Nothing -> notFound
                Just team ->
                    teamToJson teamId team
                        ["money" .= teamMoney team | userTeamId user == teamId]

teamToJson :: TeamId -> Team -> [(T.Text, Data.Aeson.Value)] -> HandlerT App IO Yesod.Value
teamToJson teamId team additionalItems =
    runDB (Core.getPlayersOrdered teamId (teamFormationId team)) >>= \playersAndFormation ->
        return $ object $ additionalItems ++ 
              ["id" .= teamId,
               "name" .= teamName team,
               "players" .= map (playerToJson . fst) playersAndFormation,
               "formation" .= mapMaybe snd playersAndFormation]

playerToJson :: Entity Player -> Data.Aeson.Value
playerToJson p =
    object ["name" .= (playerName . entityVal) p,
            "shooting" .= (playerShooting . entityVal) p,
            "passing" .= (playerPassing . entityVal) p,
            "tackling" .= (playerTackling . entityVal) p,
            "handling" .= (playerHandling . entityVal) p,
            "speed" .= (playerSpeed . entityVal) p,
            "positions" .= (playerPositionsList . entityVal) p,
            "id" .= (fromSqlKey . entityKey) p]
    where
        playerPositionsList :: Player -> [Types.FormationPitchPos]
        playerPositionsList p =
            let pos :: BS.ByteString --T.Text
                pos = playerPositions p
            in  case Data.Aeson.decodeStrict pos of
                Nothing -> []
                Just ps -> ps

getLeagueTablesR :: HandlerT App IO Yesod.Value
getLeagueTablesR = do
    authGuard showLeagueTablesFor
    where
        showLeagueTablesFor user = do
            leagues <- runDB $ selectList [{-LeagueIsFinished P.==. False-}] []
            tables <- mapM (runDB . Core.getLeagueTable . entityKey) leagues
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

getTransferListingsR :: HandlerT App IO Yesod.Value
getTransferListingsR =
    authGuard $ \user -> do
        now <- liftIO Data.Time.Clock.getCurrentTime
        ts <- runDB $ transferListingsQuery (userTeamId user) now
        return $ array $ map resultToJsonObj ts
    where
        transferListingsQuery teamId now =
            select $ from $ \(tl `LeftOuterJoin` ownBid, player) -> do
                on (
                    ((E.just $ tl ^. TransferListingId) E.==. (ownBid ?. TransferBidTransferListingId)) &&.
                    ((ownBid ?. TransferBidTeamId) E.==. (E.just $ E.val teamId))
                    )
                where_ $
                    ((tl ^. TransferListingPlayerId) E.==. (player ^. PlayerId)) &&.
                    (
                        (E.not_ $ E.isNothing $ ownBid ?. TransferBidId) E.||.
                        (tl ^. TransferListingTeamId E.==. (E.just $ E.val teamId)) E.||.
                        (
                            (tl ^. TransferListingStatus E.==. E.val Types.Active) E.&&.
                            (tl ^. TransferListingDeadline E.>. E.val now)
                        )
                    )
                return (tl, ownBid, player)

        resultToJsonObj :: (Entity TransferListing, Maybe (Entity TransferBid), Entity Player) -> Data.Aeson.Value
        resultToJsonObj (tlE, ownBidE, playerE) =
            let tl = entityVal tlE
                maybeOwnBid = fmap entityVal ownBidE
            in object [
                "id" .= (fromSqlKey . entityKey) tlE,
                "minPrice" .= transferListingMinPrice tl,
                "deadline" .= transferListingDeadline tl,
                "sellerTeamId" .= fromMaybe (toSqlKey 0) (transferListingTeamId tl),
                "status" .= case transferListingStatus tl of
                        Types.Active -> "OnSale"
                        _ -> case fmap entityKey ownBidE of
                            Nothing -> show (transferListingStatus tl)  -- 'Sold' or 'Unsold'
                            justBidId -> if justBidId == transferListingWinningBidId tl then
                                "YouWon" else "YouLost",
                "youBid" .= fmap transferBidAmount maybeOwnBid,
                "player" .= playerToJson playerE
                ]

getFixturesR :: HandlerT App IO Yesod.Value
getFixturesR =
    authGuard $ \user -> do
        fixtures <- runDB $ gamesInUserLeague user
        return $ array $ map resultToJsonObj fixtures
    where
    gamesInUserLeague user =
        select $ from $ \(g, t1, t2, teamLeague) -> do
            where_ $
                (g ^. GameHomeTeamId E.==. t1 ^. TeamId) &&.
                (g ^. GameAwayTeamId E.==. t2 ^. TeamId) &&.
                (teamLeague ^. TeamLeagueLeagueId E.==. g ^. GameLeagueId) &&.
                (teamLeague ^. TeamLeagueTeamId E.==. E.val (userTeamId user))
            orderBy [asc (g ^. GameStart)]
            limit 10000
            return (g, t1 ^. TeamName, t2 ^. TeamName)
    resultToJsonObj :: (Entity Game, E.Value T.Text, E.Value T.Text) -> Data.Aeson.Value
    resultToJsonObj (game, team1Name, team2Name) =
        object ["gameId" .= fromSqlKey (entityKey game),
                "homeName" .= unValue team1Name,
                "awayName" .= unValue team2Name,
                "start" .= gameStart (entityVal game),
                "status" .= gameStatus (entityVal game),
                "homeGoals" .= gameHomeGoals (entityVal game),
                "awayGoals" .= gameAwayGoals (entityVal game)
                ]

authGuard :: (User -> HandlerT App IO a) -> HandlerT App IO a
authGuard endpoint = getUser >>= \u -> case u of
    Nothing -> notAuthenticated
    Just user -> endpoint user

getGameEventsSinceR :: GameId -> GameEventId -> HandlerT App IO Yesod.Value
getGameEventsSinceR gameId gameEventId =
    authGuard $ \user ->
        runDB (get gameEventId) >>= \maybeGameEvent -> case maybeGameEvent of
            Nothing -> notFound
            Just gameEvent -> do
                now <- liftIO Data.Time.Clock.getCurrentTime
                game_events_and_player_names <- runDB $
                    select $ from $ \(e `LeftOuterJoin` p) -> do
                        E.on (e ^. GameEventPlayerId E.==. p ?. PlayerId)
                        where_ $
                            (e ^. GameEventGameId E.==. E.val gameId) &&.
                            (e ^. GameEventTime E.>. E.val (gameEventTime gameEvent)) &&.
                            (e ^. GameEventTime E.<=. E.val now)
                        E.orderBy [E.asc (e ^. GameEventTime)]
                        return (e, p ?. PlayerName)
                return $ array (map gameEventToJson game_events_and_player_names)

getGameEventsAllR :: GameId -> HandlerT App IO Yesod.Value
getGameEventsAllR gameId =
    authGuard $ \user -> do
        game_events_and_player_names <- runDB $
            select $ from $ \(e `LeftOuterJoin` p) -> do
                E.on (e ^. GameEventPlayerId E.==. p ?. PlayerId)
                where_ (e ^. GameEventGameId E.==. E.val gameId)
                E.orderBy [E.asc (e ^. GameEventTime)]
                return (e, p ?. PlayerName)
        return $ array (map gameEventToJson game_events_and_player_names)

getGameR :: GameId -> HandlerT App IO Yesod.Value
getGameR gameId =
    authGuard $ \user ->
        runDB (get gameId) >>= \maybeGame ->
            case maybeGame of
            Nothing -> notFound
            Just game -> gameToJson game
    where
        gameToJson game = do
            game_events_and_player_names <- runDB $
                select $ from $ \(e `LeftOuterJoin` p) -> do
                    E.on (e ^. GameEventPlayerId E.==. p ?. PlayerId)
                    where_ (e ^. GameEventGameId E.==. E.val gameId)
                    E.orderBy [E.asc (e ^. GameEventTime)]
                    return (e, p ?. PlayerName)
            maybe_home_team <- runDB $ get (gameHomeTeamId game)
            maybe_away_team <- runDB $ get (gameAwayTeamId game)
            case Just (,) <*> maybe_home_team <*> maybe_away_team of
                Nothing -> notFound
                Just (home_team, away_team) -> do
                    home_team_val <- teamToJson (gameHomeTeamId game) home_team []
                    away_team_val <- teamToJson (gameAwayTeamId game) away_team []
                    return $ object [
                        "id" .= gameId,
                        "homeTeam" .= home_team_val,
                        "awayTeam" .= away_team_val,
                        "start" .= gameStart game,
                        "status" .= gameStatus game,
                        "homeGoals" .= gameHomeGoals game,
                        "awayGoals" .= gameAwayGoals game,
                        "events" .= array (map gameEventToJson game_events_and_player_names)
                        ]

gameEventToJson :: (Entity GameEvent, E.Value (Maybe T.Text)) -> Data.Aeson.Value
gameEventToJson (e, maybePlayerName) =
    object ["id" .= fromSqlKey (entityKey e),
            "gameId" .= gameEventGameId (entityVal e),
            "kind" .= show (gameEventKind (entityVal e)),
            "side" .= if gameEventSide (entityVal e) then 1 else 0,
            "timestamp" .= gameEventTime (entityVal e),
            "message" .= fromMaybe "" (gameEventMessage (entityVal e)),
            "ballPos" .= gameEventBallPos (entityVal e),
            "playerName" .= unValue maybePlayerName
            ]

main :: IO ()
main = do
    static' <- static "static"

    -- If you have no live.db then run tests and copy test.db to live.db
    DB.getPool "live.db" 4 $ \pool ->
        liftIO $ warp 3001 $ App static' pool
