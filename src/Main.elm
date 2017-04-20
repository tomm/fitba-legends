module Main exposing (..)

import Array exposing (Array)
import Debug
import Dict exposing (Dict)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html exposing (Html, Attribute, div, input, text, ul, li, button)
import Maybe exposing (withDefault)
import Svg
import Time
import Date
import Task
import Http
import Json.Decode as Json

import Model exposing (..)
import TeamView
import FixturesView
import Styles exposing (..)


main =
    Html.program {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init =
    let formation442 : Array (Int, Int)
        formation442 = Array.fromList [
            (2, 5), -- gk
            (0, 4), (1, 4), (3, 4), (4, 4),
            (0, 2), (1, 2), (3, 2), (4, 2),
            (1, 0), (3, 0) ]
        team1 : Team
        team1 = {
            id=1,
            name="Rangers",
            players=Array.fromList [
                { name="Semen", skill=9 },
                { name="Smith", skill=4 },
                { name="Johnson", skill=2 },
                { name="Jones", skill=2 },
                { name="Morton", skill=2 },
                { name="Robertson", skill=2 },
                { name="McArse", skill=2 },
                { name="McCoist", skill=2 },
                { name="Lee", skill=2 },
                { name="Beckham", skill=2 },
                { name="Poohat", skill=2 }
            ],
            formation=formation442
        }
        team2 : Team
        team2 = {
            id=2,
            name="Celtic",
            players=Array.fromList [
                { name="Able", skill=9 },
                { name="Barber", skill=4 },
                { name="Crudley", skill=2 },
                { name="Daniels", skill=2 },
                { name="Edward", skill=2 },
                { name="Fransham", skill=2 },
                { name="Grahams", skill=2 },
                { name="Holst", skill=2 },
                { name="Ibrahim", skill=2 },
                { name="Jacek", skill=2 },
                { name="Köhl", skill=2 }
            ],
            formation=formation442
        }
        model = {
            errorMsg = Nothing,
            ourTeamId=1, tabTeamSelectedPlayer=Nothing, tab = TabTeam, ourTeam = team1,
            fixtures = [],
            leagueTables = [],
            games = Dict.fromList [
                (2, {
                        id=2, homeTeam=team1, awayTeam=team2, start=1492174154 * Time.second, events=[
                            { id=2, type_=Boring, timestamp=1492174154 * Time.second, message="Kick off!", ballPos=(2,2) },
                            { id=2, type_=Boring, timestamp=1492174156 * Time.second, message="Shit happened!", ballPos=(2,3) },
                            { id=2, type_=Boring, timestamp=1492174158 * Time.second, message="And again", ballPos=(4,3) },
                            { id=2, type_=Boring, timestamp=1492174160 * Time.second, message="The final whistle has been blown!", ballPos=(4,2) }
                        ]
                    }
                )
            ]
        }
    in
        (model, Cmd.batch [getFixtures, getLeagueTables])


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [
        Time.every Time.second ClockTick
    ]


-- HTTP

getFixtures : Cmd Msg
getFixtures =
    Http.send UpdateFixtures (Http.get "/fixtures" decodeFixturesUrl)

getLeagueTables : Cmd Msg
getLeagueTables =
    Http.send UpdateLeagueTables (Http.get "/tables" decodeTablesUrl)

jsonStringToTime : Json.Decoder Time.Time
jsonStringToTime =
  Json.string
    |> Json.andThen (\val ->
        case Date.fromString val of
          Err err -> Json.fail err
          Ok d -> Json.succeed <| Date.toTime d)

decodeFixturesUrl : Json.Decoder (List Fixture)
decodeFixturesUrl =
    Json.list (
        Json.map5 Fixture
            (Json.at ["gameId"] Json.int)
            (Json.at ["homeName"] Json.string)
            (Json.at ["awayName"] Json.string)
            (Json.at ["start"] jsonStringToTime)
            (Json.at ["status"] Json.string |> Json.andThen (\val ->
                if val == "Scheduled" then
                    Json.succeed Scheduled
                else
                    Json.succeed <| Played { homeGoals=999, awayGoals=-999 }
            ))
    )

decodeTablesUrl : Json.Decoder (List LeagueTable)
decodeTablesUrl =
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

--type alias Fixture = { id: GameId, homeName: String, awayName: String, start: Time, status: FixtureStatus }

-- UPDATE

type Msg
  = ChangeTab UiTab | TeamViewMsg TeamView.Msg | ClockTick Time.Time
  | FixturesViewMsg FixturesView.Msg
  | UpdateFixtures (Result Http.Error (List Fixture))
  | UpdateLeagueTables (Result Http.Error (List LeagueTable))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeTab tab -> ({ model | tab = tab }, Cmd.none)
    ClockTick t -> case model.tab of
        -- keep incrementing match viewing timePoint
        TabFixtures (Just watchingGame) -> ({ model | tab=TabFixtures (Just {watchingGame | timePoint =
            watchingGame.timePoint + 1 * Time.second})}, Cmd.none)
        _ -> (model, Cmd.none)
    TeamViewMsg msg -> (TeamView.update msg model, Cmd.none)
    FixturesViewMsg msg -> (FixturesView.update msg model, Cmd.none)
    UpdateFixtures result -> case result of
        Ok fixtures -> ({ model | fixtures = fixtures }, Cmd.none)
        Err error -> ({model | errorMsg = Just <| toString error}, Cmd.none)
    UpdateLeagueTables result -> case result of
        Ok tables -> ({ model | leagueTables = tables }, Cmd.none)
        Err error -> ({model | errorMsg = Just <| toString error}, Cmd.none)

-- VIEW

tabs : Model -> Html Msg
tabs model =
  let liStyle = style[("display", "block"), ("float", "left"), ("width", "25%"), ("border", "0")]
      tabStyle tab = if model.tab == tab then activeTabStyle else inactiveTabStyle
      tabLabels = [(TabTeam, "Team"), (TabLeagueTables, "Tables"), (TabFixtures Nothing, "Fixtures"), (TabFinances, "Finances")]

  in ul [style [("opacity", "0.9"), ("listStyleType", "none"), ("width", "100%"), ("padding", "0 0 1em 0"), ("top", "0"), ("left", "0"), ("margin", "0"), ("position", "fixed")]]
      (List.map (\(tab, label) ->
          li [liStyle] [button [onClick (ChangeTab tab), tabStyle tab] [text label]]
        )
        tabLabels)

view : Model -> Html Msg
view model =
  div []
    [ div [] [tabs model]
    , div [style [("clear", "both"), ("margin", "4em 0 0 0")]] [
        text <| Maybe.withDefault "" model.errorMsg,
        case model.tab of
          TabTeam -> Html.map TeamViewMsg <| TeamView.view model model.ourTeam
          TabLeagueTables -> div [] (List.map (leagueTableTab model) model.leagueTables)
          TabFixtures maybeWatchingGame -> Html.map FixturesViewMsg <| FixturesView.view model maybeWatchingGame
          TabFinances -> text ""
      ]
    ]

leagueTableTab : Model -> LeagueTable -> Html Msg
leagueTableTab model league =
    let recordToTableLine record =
        Html.tr
            [] 
            [
                Html.td [] [text record.name]
              , Html.td [] [record.won + record.drawn + record.lost |> toString |> text]
              , Html.td [] [record.won |> toString |> text]
              , Html.td [] [record.drawn |> toString |> text]
              , Html.td [] [record.lost |> toString |> text]
              , Html.td [] [record.goalsFor |> toString |> text]
              , Html.td [] [record.goalsAgainst |> toString |> text]
              , Html.td [] [record.goalsFor + record.goalsAgainst |> toString |> text]
              , Html.td [] [calcPoints record |> toString |> text]
            ]
  in div [] [
      Html.h2 [] [text <| league.name],
      Html.table [tableStyle] (
      (Html.tr [] [
        Html.th [] [text "Team"]
      , Html.th [] [text "Played"]
      , Html.th [] [text "Won"]
      , Html.th [] [text "Drawn"]
      , Html.th [] [text "Lost"]
      , Html.th [] [text "GF"]
      , Html.th [] [text "GA"]
      , Html.th [] [text "GD"]
      , Html.th [] [text "Points"]
      ]) ::
      (List.map recordToTableLine (sortLeague league.record))
    )]

calcPoints : SeasonRecord -> Int
calcPoints sr = sr.won*3 + sr.drawn*1

sortLeague : List SeasonRecord -> List SeasonRecord
sortLeague sr =
  let ptsDesc a b = if calcPoints a > calcPoints b then LT else (
      if calcPoints a < calcPoints b then GT else
        (if a.goalsFor - a.goalsAgainst > b.goalsFor - b.goalsAgainst then LT else GT)
      )
                    
  in List.sortWith ptsDesc sr
