module TeamView exposing (teamTab, update, Msg)

import Array exposing (Array)
import Dict exposing (Dict)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html exposing (Html, Attribute, div, input, text, ul, li, button)

import Model exposing (..)
import Styles exposing (..)

type Msg = SelectPlayer (Maybe Int)

teamTab : Model -> Team -> Html Msg
teamTab model team =
  let isActive i = case model.tabTeamSelectedPlayer of
          Just j -> j == i
          Nothing -> False
      playerToDiv i p =
        let clickAction = onClick (SelectPlayer <| Just i)
        in Html.tr [if isActive i then activeTableRowStyle else style []] [
             Html.td [clickAction] [text <| toString <| i + 1 ]
           , Html.td [clickAction] [text <| p.name]
           , Html.td [clickAction] [text <| toString <| p.skill]
        ]
  in div [] [
      Html.h2 [] [team.name |> text],
      Html.table [tableStyle] <|
      (Html.tr [] [Html.td [] [text "Pos."]
                 , Html.td [] [text "Name"]
                 , Html.td [] [text "Skill"]]) ::
      (List.indexedMap playerToDiv (Array.toList team.players)),
      Html.img [src "pitch.png", style [("width", "100%")]] []
      ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectPlayer (Just p) -> applySelectPlayer model p
        SelectPlayer Nothing -> { model | tabTeamSelectedPlayer = Nothing }

applySelectPlayer : Model -> Int -> Model
applySelectPlayer model p =
  case model.tabTeamSelectedPlayer of
    Nothing -> { model | tabTeamSelectedPlayer = Just p }
    Just q -> if p == q then
        { model | tabTeamSelectedPlayer = Nothing }
      else
        { model | tabTeamSelectedPlayer = Just p,
                  teams = Dict.insert model.ourTeamId (swapPlayerPositions (ourTeam model) p q) model.teams
        }

arrayDirtyGet : Int -> Array a -> a
arrayDirtyGet i arr = case Array.get i arr of
  Just v -> v
  Nothing -> Debug.crash("arrayDirtyGet failed!")

swapPlayerPositions : Team -> Int -> Int -> Team
swapPlayerPositions team p q =
  let p1 = arrayDirtyGet p team.players
      p2 = arrayDirtyGet q team.players
  in { team | players = Array.set q p1 (Array.set p p2 team.players) }
