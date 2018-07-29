module Main exposing (..)

import Array exposing (Array)
import Date
import Debug
import Dict exposing (Dict)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html exposing (Html, Attribute, div, input, text, ul, li, button)
import Http
import Json.Decode as Json
import Json.Encode as JsonEncode
import Maybe exposing (withDefault)
import Navigation
import Svg
import Task
import Time
import Notification
import Notification exposing (defaultOptions)

import Uitk
import Utils
import TransferMarket
import TransferMarketTypes
import FixturesView
import FixturesViewMsg
import TeamViewTypes
import Model exposing (..)
import Types exposing (..)
import RootMsg exposing (..)
import Styles exposing (..)
import TeamView
import ClientServer exposing (..)


main =
    Html.program {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }

init : (RootModel, Cmd Msg)
init = (
        { errorMsg=Nothing, state=Loading},
        Cmd.batch [
            getStartGameData,
            Task.perform RequestNotificationPermissionResult Notification.requestPermission
        ]
    )

-- SUBSCRIPTIONS

subscriptions : RootModel -> Sub Msg
subscriptions model = Sub.batch [
        Time.every Time.second SecondTick,
        Time.every Time.minute MinuteTick
    ]

-- UPDATE

handleHttpError : Http.Error -> RootModel -> (RootModel, Cmd Msg)
handleHttpError error model =
    case error of
        Http.BadStatus response -> 
            if response.status.code == 403 then
                (model , Navigation.load "/login")
            else
                ({model | errorMsg = Just <| toString response}, Cmd.none)
        _ -> ({model | errorMsg = Just <| toString error}, Cmd.none)

popupNotification : String -> String -> Cmd Msg
popupNotification title body =
    Notification.Notification title { defaultOptions | body = Just body }
        |> Notification.create |> Task.attempt NotificationResult

maybeNotifyGameStarting : Model -> Time.Time -> Cmd Msg
maybeNotifyGameStarting model now =
    let ownGameStarting fixture =
            (   (fixture.homeName == model.ourTeam.name) ||
                (fixture.awayName == model.ourTeam.name)
            ) && Utils.timeEqYYMMDDHHMM now (fixture.start - 5*Time.minute)
        opponent fixture =
            if fixture.homeName == model.ourTeam.name then
            fixture.awayName else fixture.homeName
        nextOwnFixture = List.head <| List.filter ownGameStarting model.fixtures
    in
        case nextOwnFixture of
            Nothing -> Cmd.none
            Just fixture -> popupNotification "Fitba" <|
                "Your game against " ++ opponent fixture ++ " will start in 5 minutes"

update : Msg -> RootModel -> (RootModel, Cmd Msg)
update msg model =
    let updateState newState = ({ model | state = GameData newState, errorMsg = Nothing }, Cmd.none)
        updateStateCmd newState cmd = ({ model | state = GameData newState, errorMsg = Nothing }, cmd)
        updateWatchingGame watchingGame newEvents =
            let game = watchingGame.game
            in {watchingGame | game = {game | events = List.append game.events newEvents} }
        handleLoadingStateMsgs =
            case msg of
                GotStartGameData result -> case result of
                    Ok team -> ({model | state=GameData {ourTeamId = team.id,
                                 tab = TabTeam { team = team, view = TeamViewTypes.SquadView { selectedPlayer = Nothing } },
                                 ourTeam = team,
                                 currentTime = Nothing,
                                 fixtures = [],
                                 leagueTables = []
                                }}, Cmd.batch [getFixtures, getLeagueTables])
                    Err error -> handleHttpError error model
                RequestNotificationPermissionResult _ -> (model, Cmd.none)
                _ -> ({model | errorMsg = Just "Unexpected message while loading ..."}, Cmd.none)
        handleActiveStateMsgs m =
            case msg of
                RequestNotificationPermissionResult _ -> (model, Cmd.none)
                NotificationResult _ -> (model, Cmd.none)
                ChangeTab tab ->
                    let cmd = case tab of
                        -- fetch updated poop
                        TabFixtures _ -> getFixtures
                        TabLeagueTables -> getLeagueTables
                        TabTeam _ -> getStartGameData
                        _ -> Cmd.none
                    in updateStateCmd { m | tab = tab } (Cmd.batch [cmd, popupNotification "Hello" "Hello nonce"])
                ViewTransferMarket -> (model, ClientServer.loadTransferListings)
                GotTransferListings result -> case result of
                    Ok listings -> updateState { m | tab = TabTransferMarket {
                        view=TransferMarketTypes.ListView, listings=listings } }
                    Err error -> handleHttpError error model
                ViewTeam teamId -> (model, ClientServer.loadTeam teamId)
                ViewTeamLoaded result -> case result of
                    Ok team -> updateState { m | tab = TabViewOtherTeam {
                        team = team,
                        view = TeamViewTypes.SquadView { selectedPlayer = Nothing }
                    } }
                    Err error -> handleHttpError error model
                SecondTick t ->
                    let (state, cmd) = FixturesView.update FixturesViewMsg.GameTick m
                        state2 = { state | currentTime = Just t }
                    in ({ model | state = GameData state2}, cmd)
                MinuteTick t ->
                    (model, maybeNotifyGameStarting m t)
                MsgTeamView msg -> case m.tab of
                    TabTeam state ->
                        let (newState, cmd) = TeamView.update msg state
                        in updateStateCmd { m | tab = TabTeam newState,
                                                ourTeam = newState.team } cmd
                    _ -> (model, Cmd.none)
                MsgFixturesView msg ->
                    let (state, cmd) = FixturesView.update msg m
                    in ({ model | state = GameData state}, cmd)
                MsgTransferMarket msg -> case m.tab of
                    TabTransferMarket state -> 
                        let (newState, cmd) = TransferMarket.update msg state
                        in updateStateCmd { m | tab = TabTransferMarket newState } cmd
                    _ -> (model, Cmd.none)
                UpdateFixtures result -> case result of
                    Ok fixtures -> updateState { m | fixtures = fixtures }
                    Err error -> handleHttpError error model
                LoadGame result -> case result of
                    Ok game -> 
                        if game.status == InProgress then
                            -- start InProgress games from latest event
                            let timeElapsed = List.length game.events -- XXX TODO hack. will only work
                                                                      -- assuming match simulated takes 1 second per event
                            in updateState { m | tab = TabFixtures (Just {
                                game=game,
                                timePoint=Time.second * (toFloat timeElapsed)
                            })}
                        else
                            -- start Played and Scheduled games from beginning
                            updateState { m | tab = TabFixtures (Just { game=game, timePoint=0.0}) }
                    Err error -> handleHttpError error model
                UpdateGame result -> case result of
                    Ok events -> case m.tab of
                        TabFixtures (Just watchingGame) -> updateState {
                            m | tab=TabFixtures (Just <| updateWatchingGame watchingGame events)
                        }
                        _ -> (model, Cmd.none)
                    Err error -> handleHttpError error model
                UpdateLeagueTables result -> case result of
                    Ok tables -> updateState { m | leagueTables = tables }
                    Err error -> handleHttpError error model
                GotStartGameData result -> case result of
                    Ok team -> updateState { m | ourTeam = team }
                    Err error -> handleHttpError error model
                SavedFormation _ -> (model, Cmd.none) -- don't give a fuck
                SavedBid _ -> (model, Cmd.none) -- don't give a fuck
                SellPlayerResponse _ -> (model, ClientServer.loadTransferListings) -- don't give a fuck
                NoOp -> (model, Cmd.none)

    in
        case model.state of
            Loading -> handleLoadingStateMsgs
            GameData m -> handleActiveStateMsgs m


-- VIEW

tabs : Model -> Html Msg
tabs model =
  let liStyle = style[("display", "block"), ("float", "left"), ("width", "25%"), ("border", "0")]
      tabStyle tab = if model.tab == tab then activeTabStyle else inactiveTabStyle
      tabLabels = [(TabTeam { team = model.ourTeam, view = TeamViewTypes.SquadView { selectedPlayer = Nothing } }, "Team"),
                   (TabLeagueTables, "Tables"),
                   (TabFixtures Nothing, "Fixtures"),
                   (TabFinances, "Finances")]

  in ul [style [("opacity", "0.9"), ("listStyleType", "none"), ("width", "100%"), ("padding", "0 0 1em 0"), ("top", "0"), ("left", "0"), ("margin", "0"), ("position", "fixed")]]
      (List.map (\(tab, label) ->
          li [liStyle] [button [onClick (ChangeTab tab), tabStyle tab] [text label]]
        )
        tabLabels)

view : RootModel -> Html Msg
view model =
    div [] [
        case model.state of
            Loading -> text <| Maybe.withDefault "Loading ..." model.errorMsg
            GameData m ->
                div [] [
                    tabs m,
                    div [style [("clear", "both"), ("margin", "4em 0 0 0")]] [
                        text <| Maybe.withDefault "" model.errorMsg,
                        case m.tab of
                            TabViewOtherTeam state -> Html.map (\_ -> NoOp {- can't edit -}) <| TeamView.view state
                            TabTeam state -> Html.map MsgTeamView <| TeamView.view state
                            TabLeagueTables -> div [] (List.map (leagueTableTab m) m.leagueTables)
                            TabFixtures maybeWatchingGame -> Html.map MsgFixturesView <| FixturesView.view m maybeWatchingGame
                            TabFinances -> financesTab m
                            TabTransferMarket state -> Html.map MsgTransferMarket <| TransferMarket.view m.ourTeamId state
                        ]
                ]
    ]

financesTab : Model -> Html Msg
financesTab model =
    Uitk.view Nothing "Finances" [
        div [] [text <| "You have " ++ (Utils.moneyFormat <| Maybe.withDefault 0 model.ourTeam.money) ++ " available for transfers."],
        Uitk.actionButton ViewTransferMarket "Transfer Market"
    ]

leagueTableTab : Model -> LeagueTable -> Html Msg
leagueTableTab model league =
    let recordToTableLine index record =
        Html.tr
            [onClick (ViewTeam record.teamId)] 
            [
                Html.td [] [text <| toString <| index + 1]
              , Html.td [] [text record.name]
              , Html.td [] [record.won + record.drawn + record.lost |> toString |> text]
              , Html.td [] [record.won |> toString |> text]
              , Html.td [] [record.drawn |> toString |> text]
              , Html.td [] [record.lost |> toString |> text]
              , Html.td [] [record.goalsFor |> toString |> text]
              , Html.td [] [record.goalsAgainst |> toString |> text]
              , Html.td [] [record.goalsFor - record.goalsAgainst |> toString |> text]
              , Html.td [] [calcPoints record |> toString |> text]
            ]
    in
        Uitk.view Nothing league.name [
            Html.table [class "league-table"] (
                (Html.tr [] [
                    Html.th [] [text "Pos."]
                  , Html.th [] [text "Team"]
                  , Html.th [] [text "Played"]
                  , Html.th [] [text "Won"]
                  , Html.th [] [text "Drawn"]
                  , Html.th [] [text "Lost"]
                  , Html.th [] [text "GF"]
                  , Html.th [] [text "GA"]
                  , Html.th [] [text "GD"]
                  , Html.th [] [text "Points"]
                ]) ::
                (List.indexedMap recordToTableLine (sortLeague league.record))
            )
        ]

calcPoints : SeasonRecord -> Int
calcPoints sr = sr.won*3 + sr.drawn*1

sortLeague : List SeasonRecord -> List SeasonRecord
sortLeague sr =
  let ptsDesc a b = if calcPoints a > calcPoints b then LT else (
      if calcPoints a < calcPoints b then GT else
        (if a.goalsFor - a.goalsAgainst > b.goalsFor - b.goalsAgainst then LT else GT)
      )
                    
  in List.sortWith ptsDesc sr
