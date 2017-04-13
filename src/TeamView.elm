module TeamView exposing (teamTab, update, Msg)

import Array exposing (Array)
import Dict exposing (Dict)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html exposing (Html, Attribute, div, input, text, ul, li, button)
import Svg
import Svg.Events
import Svg.Attributes exposing (..)

import Model exposing (..)
import Styles exposing (..)

type Msg = SelectPlayer (Maybe Int)

pitchX = 812
pitchY = 1280

teamTab : Model -> Team -> Html Msg
teamTab model team =
  let isActive i = case model.tabTeamSelectedPlayer of
          Just j -> j == i
          Nothing -> False
      playerToDiv i p =
        let clickAction = onClick (SelectPlayer <| Just i)
        in Html.tr [if isActive i then activeTableRowStyle else Html.Attributes.style []] [
             Html.td [clickAction] [text <| toString <| i + 1 ]
           , Html.td [clickAction] [text <| p.name]
           , Html.td [clickAction] [text <| toString <| p.skill]
        ]
    in
        div [] [
            Html.h2 [] [team.name |> text],
            Html.table
                [tableStyle] <|
                (
                    Html.tr [] [Html.th [] [text "Pos."],
                    Html.th [] [text "Name"],
                    Html.th [] [text "Skill"]]
                ) ::
                (List.indexedMap playerToDiv (Array.toList team.players)),

            Svg.svg
                [Svg.Attributes.width "100%", Svg.Attributes.height "100%", viewBox "0 0 812 1280" ]
                ([ 
                -- Svg.rect [ x "10", y "10", Svg.Attributes.width "100", Svg.Attributes.height "100", rx "15", ry "15" ] [],
                Svg.image
                    [ Svg.Events.onClick <| SelectPlayer Nothing,
                      Svg.Attributes.width "100%", Svg.Attributes.height "100%", Svg.Attributes.xlinkHref "pitch.png" ]
                    []
                ]
                ++
                List.indexedMap (\i (x,y) -> playerOnPitch model team i x y) team.formation
                )
        ]

playerOnPitch : Model -> Team -> Int -> Int -> Int -> Svg.Svg Msg
playerOnPitch model team playerIdx x y =
    let maybePlayer = Array.get playerIdx team.players
        label =
            case maybePlayer of
                Nothing -> ("Empty!", "red")
                Just player -> (player.name, if model.tabTeamSelectedPlayer == Just playerIdx then "#8080ff" else "white")

        textAtPlayerPos : (String, String) -> Int -> Int -> Svg.Svg Msg
        textAtPlayerPos (str, color) x y =
            let
                xpadding = 100.0
                ypadding = 250.0
                xinc = (pitchX - 2*xpadding) / 4
                yinc = (pitchY - 2*ypadding) / 4.3
                xpos = xpadding + (toFloat x)*xinc
            in
                Svg.text_
                    [ Svg.Events.onClick (SelectPlayer (Just playerIdx)),
                      Svg.Attributes.textAnchor "middle", fill color,
                      Svg.Attributes.x (toString xpos), Svg.Attributes.y (toString (ypadding + (toFloat y)*yinc)), Svg.Attributes.fontSize "30" ]
                    [
                        Svg.tspan [Svg.Attributes.x <|toString xpos, dy "0"] [Svg.text <| toString (playerIdx+1) ],
                        Svg.tspan [Svg.Attributes.x <|toString xpos, dy "40"] [Svg.text str ]
                    ]
    in
        textAtPlayerPos label x y


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
