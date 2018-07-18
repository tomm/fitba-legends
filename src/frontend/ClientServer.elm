module ClientServer exposing (loadGame, loadTeam, saveFormation, sellPlayer, pollGameEvents, getStartGameData, getFixtures, getLeagueTables,
    loadTransferListings, makeTransferBid)

import Array exposing (Array)
import Date
import Http
import Json.Decode as Json
import Json.Encode
import Time

import Model exposing (..)
import RootMsg exposing (Msg, Msg(..))
import Types exposing (..)

loadGame : GameId -> Cmd Msg
loadGame gameId =
    let url = "/game_events/" ++ toString gameId
    in Http.send LoadGame (Http.get url jsonDecodeGame)

sellPlayer : PlayerId -> Cmd Msg
sellPlayer playerId =
    let body = Http.jsonBody <| Json.Encode.object [("player_id", Json.Encode.int playerId)]
    in Http.send SellPlayerResponse (Http.post "/sell_player" body Json.string)

saveFormation : Team -> Cmd Msg
saveFormation team = 
    let tuplePlayerPos idx player =
            case Array.get idx team.formation of
                Nothing -> Json.Encode.list [ Json.Encode.int player.id, Json.Encode.null ]
                Just pos -> Json.Encode.list [
                    Json.Encode.int player.id,
                    Json.Encode.list [ Json.Encode.int <| Tuple.first pos, Json.Encode.int <| Tuple.second pos ]
                    ]
        body = Http.jsonBody <| Json.Encode.array <| Array.indexedMap tuplePlayerPos team.players
    in
        Http.send SavedFormation (Http.post "/save_formation" body Json.string)

makeTransferBid : TransferListingId -> Maybe Int -> Cmd Msg
makeTransferBid tid amount =
    let body = Http.jsonBody <| Json.Encode.object [
            ("amount", case amount of
                Just a -> Json.Encode.int a
                Nothing -> Json.Encode.null),
            ("transfer_listing_id", Json.Encode.int tid)
        ]
    in Http.send SavedBid (Http.post "/transfer_bid" body Json.string)

loadTransferListings : Cmd Msg
loadTransferListings = Http.send GotTransferListings (Http.get "/transfer_listings" jsonDecodeTransferListings)

loadTeam : TeamId -> Cmd Msg
loadTeam teamId =
    let url = "/squad/" ++ toString teamId
    in Http.send ViewTeamLoaded (Http.get url jsonDecodeTeam)

pollGameEvents : GameId -> Maybe GameEventId -> Cmd Msg
pollGameEvents gameId lastEventId =
    let url = "/game_events_since/" ++ toString gameId ++ "/" ++
        case lastEventId of
            Just eventId -> toString eventId
            Nothing -> ""
    in Http.send UpdateGame (Http.get url <| Json.list jsonDecodeGameEvent)

getStartGameData : Cmd Msg
getStartGameData = Http.send GotStartGameData (Http.get "/load_world" jsonDecodeTeam)

getFixtures : Cmd Msg
getFixtures =
    Http.send UpdateFixtures (Http.get "/fixtures" jsonDecodeFixtures)

getLeagueTables : Cmd Msg
getLeagueTables =
    Http.send UpdateLeagueTables (Http.get "/tables" jsonDecodeLeagueTables)

jsonDecodeTransferListings : Json.Decoder (List TransferListing)
jsonDecodeTransferListings =
    Json.list (
        Json.map7 TransferListing
            (Json.field "id" Json.int)
            (Json.field "minPrice" Json.int)
            (Json.field "deadline" jsonDecodeTime)
            (Json.field "sellerTeamId" Json.int)
            (Json.field "player" jsonDecodePlayer)
            (Json.field "youBid" Json.int |> Json.maybe)
            (Json.field "status" Json.string |> Json.andThen
                (\val ->
                    case val of
                        "OnSale" -> Json.succeed OnSale
                        "YouWon" -> Json.succeed YouWon
                        "YouLost" -> Json.succeed YouLost
                        "Sold" -> Json.succeed Sold
                        "Unsold" -> Json.succeed Unsold
                        _ -> Json.fail <| "Unexpected TransferListing status: " ++ val
                )
            )
    )

jsonDecodeGame : Json.Decoder Game
jsonDecodeGame =
    Json.map6 Game
        (Json.field "id" Json.int)
        (Json.field "homeTeam" jsonDecodeTeam)
        (Json.field "awayTeam" jsonDecodeTeam)
        (Json.field "start" jsonDecodeTime)
        (Json.field "events" <| Json.list jsonDecodeGameEvent)
        (Json.at ["status"] Json.string |> Json.andThen (\val ->
            case val of
                "Scheduled" -> Json.succeed Scheduled
                "InProgress" -> Json.succeed InProgress
                "Played" -> Json.map Played
                    (Json.map2 FixtureStatusPlayed
                        (Json.at ["homeGoals"] Json.int)
                        (Json.at ["awayGoals"] Json.int)
                    )
                _ -> Json.fail <| "Unexpected fixture status: " ++ val
        ))

jsonDecodeGameEvent : Json.Decoder GameEvent
jsonDecodeGameEvent =
    Json.map8 GameEvent
        (Json.field "id" Json.int)
        (Json.field "gameId" Json.int)
        (Json.field "kind" jsonDecodeGameEventKind)
        (Json.field "side" jsonDecodeGameEventSide)
        (Json.field "timestamp" jsonDecodeTime)
        (Json.field "message" Json.string)
        (Json.field "ballPos" jsonDecodePlayerPosition)
        (Json.field "playerName" Json.string |> Json.maybe)

jsonDecodeGameEventKind : Json.Decoder GameEventKind
jsonDecodeGameEventKind =
    Json.string |> Json.andThen (\val ->
        case val of
            "KickOff" -> Json.succeed KickOff
            "Goal" -> Json.succeed Goal
            "Boring" -> Json.succeed Boring
            "ShotTry" -> Json.succeed ShotTry
            "ShotMiss" -> Json.succeed ShotMiss
            "ShotSaved" -> Json.succeed ShotSaved
            "EndOfGame" -> Json.succeed EndOfGame
            _ -> Json.fail ("Invalid GameEvent kind: " ++ val)
        )

jsonDecodeGameEventSide : Json.Decoder GameEventSide
jsonDecodeGameEventSide =
    Json.int |> Json.andThen (\val ->
        case val of
            0 -> Json.succeed Home
            1 -> Json.succeed Away
            _ -> Json.fail ("Invalid GameEvent side: " ++ toString val)
        )

jsonDecodeTeam : Json.Decoder Team
jsonDecodeTeam =
    Json.map5 Team
        (Json.field "id" Json.int)
        (Json.field "name" Json.string)
        (Json.field "players" (Json.array jsonDecodePlayer))
        (Json.field "formation" (Json.array jsonDecodePlayerPosition))
        (Json.field "money" Json.int |> Json.maybe)

jsonDecodePlayer : Json.Decoder Player
jsonDecodePlayer =
    Json.map8 Player
        (Json.field "id" Json.int)
        (Json.field "name" Json.string)
        (Json.field "shooting" Json.int)
        (Json.field "passing" Json.int)
        (Json.field "tackling" Json.int)
        (Json.field "handling" Json.int)
        (Json.field "speed" Json.int)
        (Json.field "positions" (Json.list jsonDecodePlayerPosition))

jsonDecodePlayerPosition : Json.Decoder (Int, Int)
jsonDecodePlayerPosition =
    (Json.array Json.int) |> Json.andThen (\val ->
        let mx = Array.get 0 val 
            my = Array.get 1 val
        in case (mx,my) of
            (Just x, Just y) -> Json.succeed (x, y)
            _ -> Json.fail "Expected Int,Int"
    )

jsonDecodeTime : Json.Decoder Time.Time
jsonDecodeTime =
  Json.string
    |> Json.andThen (\val ->
        case Date.fromString val of
          Err err -> Json.fail err
          Ok d -> Json.succeed <| Date.toTime d)

jsonDecodeFixtures : Json.Decoder (List Fixture)
jsonDecodeFixtures =
    Json.list (
        Json.map5 Fixture
            (Json.at ["gameId"] Json.int)
            (Json.at ["homeName"] Json.string)
            (Json.at ["awayName"] Json.string)
            (Json.at ["start"] jsonDecodeTime)
            (Json.at ["status"] Json.string |> Json.andThen (\val ->
                case val of
                    "Scheduled" -> Json.succeed Scheduled
                    "InProgress" -> Json.succeed InProgress
                    "Played" -> Json.map Played
                        (Json.map2 FixtureStatusPlayed
                            (Json.at ["homeGoals"] Json.int)
                            (Json.at ["awayGoals"] Json.int)
                        )
                    _ -> Json.fail <| "Unexpected fixture status: " ++ val
            ))
    )

jsonDecodeLeagueTables : Json.Decoder (List LeagueTable)
jsonDecodeLeagueTables =
    Json.list (
        Json.map2 LeagueTable
            (Json.at ["name"] Json.string)
            (Json.at ["record"] <| Json.list (
                Json.map8 SeasonRecord
                    (Json.at ["teamId"] Json.int)
                    (Json.at ["name"] Json.string)
                    (Json.at ["played"] Json.int)
                    (Json.at ["won"] Json.int)
                    (Json.at ["drawn"] Json.int)
                    (Json.at ["lost"] Json.int)
                    (Json.at ["goalsFor"] Json.int)
                    (Json.at ["goalsAgainst"] Json.int)
            ))
    )
