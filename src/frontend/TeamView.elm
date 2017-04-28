module TeamView exposing (view, update)

import Array exposing (Array)
import Dict exposing (Dict)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html exposing (Html, Attribute, div, input, text, ul, li, button)
import Svg
import Svg.Events
import Svg.Attributes exposing (..)

import Cmds
import Model exposing (..)
import RootMsg
import Styles exposing (..)
import TeamViewMsg exposing (Msg, Msg(SelectPlayer, MovePosition))

pitchX = 812
pitchY = 1280

-- All non-GK pitch positions. note y=0 (opposition goal line) and y=6 (own goal line) are not permitted
movablePitchPositions : List (Int, Int)
movablePitchPositions = List.concat <| List.map (\x -> List.map (\y -> (x,y)) [1,2,3,4,5]) [0,1,2,3,4]

view : Model -> Team -> Html Msg
view model team =
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
                          Svg.Attributes.width "100%", Svg.Attributes.height "100%", Svg.Attributes.xlinkHref "/static/pitch.png" ]
                        []
                ] ++
                -- players
                (Array.toList <| Array.indexedMap (\i (x,y) -> playerOnPitch model team i x y) team.formation)
                ++
                -- pitch positions unused by our formation
                case model.tabTeamSelectedPlayer of
                    Nothing -> []
                    Just 0 -> [] -- can't move goalkeeper
                    Just _ ->
                        List.map
                            (\(x, y) ->
                                    -- XXX why doesn't Array.member exist!
                                    if List.member (x, y) <| Array.toList team.formation
                                    then Svg.text ""
                                    else emptyPitchPosition (x, y)
                            )
                            movablePitchPositions
                        )
        ]

positionCircleRadius = 75

pitchPosPixelPos : (Int, Int) -> (Float, Float)
pitchPosPixelPos (x, y) =
    let
        xpadding = 100.0
        ypadding = 100.0
        xinc = (pitchX - 2*xpadding) / 4
        yinc = (pitchY - 2*ypadding) / 6
    in
        (xpadding + (toFloat x)*xinc, ypadding + (toFloat y)*yinc)

emptyPitchPosition : (Int, Int) -> Svg.Svg Msg
emptyPitchPosition (x, y) =
    let
        (xpos, ypos) = pitchPosPixelPos (x, y)
    in
        Svg.circle [ Svg.Events.onClick (MovePosition (x, y)),
                     cx (toString xpos), cy (toString ypos), r <| toString positionCircleRadius, fill "black", fillOpacity "0.1" ] []

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
                (xpos, ypos) = pitchPosPixelPos (x, y)
            in
                Svg.g
                    []
                    [ Svg.circle
                        [ Svg.Events.onClick (SelectPlayer (Just playerIdx)),
                          cx (toString xpos), cy (toString ypos), r <| toString positionCircleRadius, fill "black", fillOpacity "0.1" ]
                        []
                    , Svg.text_
                        [ Svg.Events.onClick (SelectPlayer (Just playerIdx)),
                          Svg.Attributes.textAnchor "middle", fill color,
                          Svg.Attributes.x (toString xpos), Svg.Attributes.y (toString ypos), Svg.Attributes.fontSize "30" ]
                        [
                            Svg.tspan [Svg.Attributes.x <|toString xpos, dy "0"] [Svg.text <| toString (playerIdx+1) ],
                            Svg.tspan [Svg.Attributes.x <|toString xpos, dy "40"] [Svg.text str ]
                        ]
                    ]
    in
        textAtPlayerPos label x y


update : Msg -> Model -> (Model, Cmd RootMsg.Msg)
update msg model =
    case msg of
        SelectPlayer (Just p) ->
            let (newModel, changed) = applySelectPlayer model p
            in (newModel, if changed then Cmds.saveFormation <| newModel.ourTeam else Cmd.none)
        SelectPlayer Nothing -> ({ model | tabTeamSelectedPlayer = Nothing }, Cmd.none)
        -- move selected player to new position
        MovePosition pos ->
            case model.tabTeamSelectedPlayer of
                Nothing -> (model, Cmd.none)
                Just playerIdx ->
                    let newTeam = movePlayerPosition model.ourTeam playerIdx pos
                    in if newTeam /= model.ourTeam then
                        ({model | ourTeam = newTeam, tabTeamSelectedPlayer = Nothing},
                         Cmds.saveFormation <| newTeam)
                       else ({model | tabTeamSelectedPlayer = Nothing}, Cmd.none)

movePlayerPosition : Team -> Int -> (Int, Int) -> Team
movePlayerPosition team playerIdx pos =
    -- can't move the goalkeeper!
    if playerIdx == 0 then
        team
    else
        { team | formation = Array.set playerIdx pos team.formation }

applySelectPlayer : Model -> Int -> (Model, Bool)
applySelectPlayer model p =
  case model.tabTeamSelectedPlayer of
    Nothing -> ({ model | tabTeamSelectedPlayer = Just p }, False)
    Just q -> if p == q then
        ({ model | tabTeamSelectedPlayer = Nothing }, False)
      else
        ({ model | tabTeamSelectedPlayer = Nothing,
                   ourTeam = swapPlayerPositions (model.ourTeam) p q
        }, True)

arrayDirtyGet : Int -> Array a -> a
arrayDirtyGet i arr = case Array.get i arr of
  Just v -> v
  Nothing -> Debug.crash("arrayDirtyGet failed!")

swapPlayerPositions : Team -> Int -> Int -> Team
swapPlayerPositions team p q =
  let p1 = arrayDirtyGet p team.players
      p2 = arrayDirtyGet q team.players
  in { team | players = Array.set q p1 (Array.set p p2 team.players) }
