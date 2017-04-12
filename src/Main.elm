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

import Model exposing (..)
import TeamView
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
    let teams : Dict TeamId Team
        teams = Dict.fromList[
            (1, {id=1, name="Rangers", players=Array.fromList [
                { name="A", skill=9 },
                { name="B", skill=4 },
                { name="C", skill=2 }
            ]}),
            (2, {id=2, name="Celtic", players=Array.empty})
        ]
        model = { ourTeamId=1, tabTeamSelectedPlayer=Nothing, tab = TabTeam, teams = teams }
    in
        (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [
        Time.every Time.second ClockTick
    ]

-- UPDATE

type Msg
  = ChangeTab UiTab | TeamViewMsg TeamView.Msg | ClockTick Time.Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeTab tab -> ({ model | tab = tab }, Cmd.none)
    ClockTick t -> (model, Cmd.none)
    TeamViewMsg msg -> (TeamView.update msg model, Cmd.none)

-- VIEW

tabs : Model -> Html Msg
tabs model =
  let liStyle = style[("display", "block"), ("float", "left"), ("width", "25%")]
      tabStyle tab = if model.tab == tab then activeTabStyle else inactiveTabStyle
      tabLabels = [(TabTeam, "Team"), (TabLeagueTables, "Tables"), (TabFixtures, "Fixtures"), (TabFinances, "Finances")]

  in ul [style [("listStyleType", "none"), ("padding", "0 0 1em 0"), ("margin", "0")]]
      (List.map (\(tab, label) ->
          li [liStyle] [button [onClick (ChangeTab tab), tabStyle tab] [text label]]
        )
        tabLabels)

view : Model -> Html Msg
view model =
  div []
    [ div [] [tabs model]
    , div [style [("clear", "both"), ("margin", "2em 0 0 0")]] [
        case model.tab of
          TabTeam -> case Dict.get model.ourTeamId model.teams of
                     Just team -> Html.map TeamViewMsg <| TeamView.teamTab model team
                     Nothing -> text "Error: unknown teamId"
          TabLeagueTables -> leagueTableTab model premierLeague
          _ -> text "Arse"
      ]
    ]

leagueTableTab : Model -> LeagueTable -> Html Msg
leagueTableTab model league =
  let recordToTableLine record =
    Html.tr [] (case Dict.get record.teamId model.teams of
      Nothing -> [Html.td [] [text "Error. Unknown teamId"]]
      Just team -> [
        Html.td [] [text team.name]
      , Html.td [] [record.won + record.drawn + record.lost |> toString |> text]
      , Html.td [] [record.won |> toString |> text]
      , Html.td [] [record.drawn |> toString |> text]
      , Html.td [] [record.lost |> toString |> text]
      , Html.td [] [record.goalsFor |> toString |> text]
      , Html.td [] [record.goalsAgainst |> toString |> text]
      , Html.td [] [record.goalsFor + record.goalsAgainst |> toString |> text]
      , Html.td [] [calcPoints record |> toString |> text]
      ])
  in div [] [
      Html.h2 [] [text <| league.name],
      Html.table [tableStyle] (
      (Html.tr [] [
        Html.td [] [text "Team"]
      , Html.td [] [text "Played"]
      , Html.td [] [text "Won"]
      , Html.td [] [text "Drawn"]
      , Html.td [] [text "Lost"]
      , Html.td [] [text "GF"]
      , Html.td [] [text "GA"]
      , Html.td [] [text "GD"]
      , Html.td [] [text "Points"]
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
