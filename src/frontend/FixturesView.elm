module FixturesView exposing (view, update)

import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html exposing (Html, Attribute, div, h2, input, table, tr, td, th, text, ul, li, button)
import Svg
import Svg.Attributes exposing (..)
import Time exposing (Time)

import Styles
import Model exposing (..)
import Utils
import Dict
import FixturesViewMsg exposing (Msg, Msg(Watch))

view : Model -> Maybe WatchingGame -> Html Msg
view model maybeWatchingGame =
    div [] (
        case maybeWatchingGame of
            Nothing -> [
                h2 [] [text "Fixtures"],
                fixturesTable model
            ]
            Just watching -> [
                h2 [] [text "Match Video"],
                case Dict.get watching.gameId model.games of
                    Nothing -> div [] [text "Game replay not available"]
                    Just game -> matchView game watching
            ]
    )

latestGameEventAtTimepoint : Game -> Time -> Maybe GameEvent
latestGameEventAtTimepoint game time =
    List.filter (\a -> a.timestamp <= time) game.events
        |> List.sortWith (\a b -> if a.timestamp > b.timestamp then LT else GT)
        |> List.head

matchView : Game -> WatchingGame -> Html Msg
matchView game watching =
    let maybeEv = latestGameEventAtTimepoint game (game.start + watching.timePoint)
        matchMinute = case maybeEv of
            Nothing -> watching.timePoint / Time.second
            Just ev -> (ev.timestamp - game.start) / Time.second
        matchTimeDisplay = div
            [Html.Attributes.style [("float", "right")]]
            [text <| (++) "Time: " <| toString <| matchMinute, text ":00"]
    in
        div []
            [
                Html.h3 [] [text (game.homeTeam.name ++ " - " ++ game.awayTeam.name)],
                matchTimeDisplay,
                (case latestGameEventAtTimepoint game (game.start + watching.timePoint) of
                    Nothing -> div [] [
                        text "The match has started!",
                        drawPitch game Nothing
                        ]
                    Just ev -> div [] [
                        text ev.message,
                        drawPitch game <| Just ev
                    ]
                )
            ]

drawPitch : Game -> (Maybe GameEvent) -> Svg.Svg Msg
drawPitch game maybeEv =
    Svg.svg
        [Svg.Attributes.width "100%", Svg.Attributes.height "100%", viewBox "0 0 812 515" ]
        ([ 
            Svg.image
                [Svg.Attributes.width "100%", Svg.Attributes.height "100%", Svg.Attributes.xlinkHref "/static/pitch_h.png" ]
                []
        ] ++
            (case maybeEv of
                Nothing -> []
                Just ev -> [drawBall ev.ballPos]
            )
            --(List.map drawBall <| List.concat <| List.map (\x -> List.map (\y -> (x,y)) [0,1,2,3,4])
            --[0,1,2,3,4])
            --++ [drawBall (2,5)]
        )

pitchX = 812
pitchY = 515
pitchPosPixelPos : (Int, Int) -> (Float, Float)
pitchPosPixelPos (x, y) =
    let
        xpadding = 150.0
        ypadding = 50.0
        xinc = (pitchX - 2*xpadding) / 4
        yinc = (pitchY - 2*ypadding) / 4
    in
        (xpadding + (toFloat (4-y))*xinc, ypadding + (toFloat x)*yinc)

drawBall : (Int, Int) -> Svg.Svg Msg
drawBall (x, y) =
    let
        (xpos, ypos) = pitchPosPixelPos (x, y)
    in
        Svg.g [] [
            Svg.circle [ cx (toString xpos), cy (toString ypos), r "30", fill "white" ] [],
            Svg.text_ [fill "black", Svg.Attributes.x <| toString xpos, Svg.Attributes.y <| toString ypos]
                      [Svg.text ((toString x) ++ "," ++ (toString y))]
        ]


fixturesTable : Model -> Html Msg
fixturesTable model =
    let fixtureRow fixture =
            tr [onClick (Watch fixture.gameId)] [
                td [] [text (fixture.homeName ++ " - " ++ fixture.awayName)],
                td [] [text <| Utils.timeFormat fixture.start],
                td [] [resultText fixture ]
            ]
        resultText fixture =
            case fixture.status of
                Scheduled -> text "Scheduled"
                Played result -> text (toString result.homeGoals ++ " : " ++ toString result.awayGoals)
    in
        table
            [Styles.tableStyle]
            ([
                tr [] [
                    th [] [text "Game"],
                    th [] [text "Date"],
                    th [] [text "Result"]
                ]
            ] ++
            List.map fixtureRow model.fixtures
            )

update : Msg -> Model -> Model
update msg model =
    case msg of
        Watch gameId -> { model | tab = TabFixtures (Just { gameId=gameId, timePoint=0.0 }) }
