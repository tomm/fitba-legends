module FixturesView exposing (view, update)

import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html exposing (Html, Attribute, div, input, span, table, tr, td, th, text, ul, h3, li, button)
import Svg
import Date
import Svg.Attributes exposing (..)
import Time exposing (Time)

import Styles
import Model exposing (..)
import Utils
import Dict
import RootMsg
import FixturesViewMsg exposing (Msg, Msg(..))
import ClientServer
import Types exposing (..)
import Uitk

match_length_seconds : Float
match_length_seconds = 270.0  -- make sure this matches app/simulation.rb:MATCH_LENGTH_SECONDS

view : Model -> Maybe WatchingGame -> Html Msg
view model maybeWatchingGame =
    case maybeWatchingGame of
        Nothing -> Uitk.view Nothing "Fixtures" [ fixturesTable model ]
        Just watching ->
            let status = case watching.game.status of
                    Scheduled -> "Live match"
                    InProgress -> "Live match"
                    Played _ -> "Match replay"
                button = case watching.game.status of
                    Played _ -> 
                        if watching.timePoint < match_length_seconds * Time.second then
                            Just <| Html.div [Html.Attributes.class "game-button-jump-to-end"]
                                                [ Uitk.actionButton ShowFinalScore "Show final score" ]
                        else Nothing
                    _ -> Nothing
            in Uitk.view button status [matchView watching]

eventsUpToTimepoint : Game -> Time -> List GameEvent
eventsUpToTimepoint game time =
    List.filter (\a -> a.timestamp <= time) game.events
        |> List.sortWith (\a b -> if a.timestamp > b.timestamp then LT else GT)

latestGameEventAtTimepoint : Game -> Time -> Maybe GameEvent
latestGameEventAtTimepoint game time = eventsUpToTimepoint game time |> List.head

secondsToMatchMinute : Float -> String
secondsToMatchMinute s =
    (toString << round) (90.0 * s / (match_length_seconds * Time.second))

goalSummary : Game -> Time -> Html Msg
goalSummary game time =
    let allGoals side = List.filter (\a -> a.timestamp <= time) game.events
            |> List.filter (\e -> e.kind == Goal && e.side == side)
            |> List.sortWith (\a b -> if a.timestamp > b.timestamp then GT else LT)
        summarizeEvent e =
            let scorer = Maybe.withDefault "" e.playerName
                when = secondsToMatchMinute (e.timestamp - game.start)
            in div [] [text <| case e.side of
                    Home -> scorer ++ " " ++ when
                    Away -> when ++ " " ++ scorer
                ]
        goalSummary side =
            div [Html.Attributes.class "half-width"] 
                [div [Html.Attributes.class "game-summary",
                      Html.Attributes.class <| "game-summary-" ++ toString side]
                     <| List.map summarizeEvent (allGoals side)
                 , Uitk.nbsp ]
    in div [] [
        Html.h4 [Html.Attributes.class "game-summary-title"] [text "Goal Summary:"],
        goalSummary Home,
        goalSummary Away
        ]

matchView : WatchingGame -> Html Msg
matchView watching =
    let game = watching.game
        maybeEv = latestGameEventAtTimepoint game (game.start + watching.timePoint)
        matchMinute = case maybeEv of
            Nothing -> secondsToMatchMinute watching.timePoint
            Just ev -> secondsToMatchMinute (ev.timestamp - game.start)
        matchTimeDisplay = if matchStarted then
                div [Html.Attributes.class "game-time"] [text <| (++) "Time: " <| matchMinute ++ ":00"]
            else
                div [] []
        all_goals = eventsUpToTimepoint game (game.start + watching.timePoint)
                        |> List.filter (\e -> e.kind == Goal)
        goals = (List.filter (\e -> e.side == Home) all_goals |> List.length,
                 List.filter (\e -> e.side == Away) all_goals |> List.length)
        matchStarted = List.length game.events > 0
        matchEventMessage e = Html.div [Html.Attributes.class "game-message",
                                        Html.Attributes.class <| "game-message-" ++ toString e.side]
                                       [text e.message, Uitk.nbsp]

    in
        div []
            [
                Html.h3 [] [text (
                    game.homeTeam.name
                    ++ " (" ++ (toString <| Tuple.first goals) ++ " : " ++ (toString <| Tuple.second goals) ++ ") "
                    ++ game.awayTeam.name 
                )],
                (case latestGameEventAtTimepoint game (game.start + watching.timePoint) of
                    Nothing -> div [] [
                        text <| "The match has not started: kick-off on " ++ Utils.timeFormat game.start,
                        drawPitch game Nothing
                        ]
                    Just ev -> div [] [
                        matchTimeDisplay,
                        div [] [matchEventMessage ev],
                        drawPitch game <| Just ev
                    ]
                ),
                goalSummary game (game.start + watching.timePoint)
            ]

drawPitch : Game -> (Maybe GameEvent) -> Svg.Svg Msg
drawPitch game maybeEv =
    Svg.svg
        [Svg.Attributes.width "100%", Svg.Attributes.height "100%", viewBox "0 0 812 515" ]
        ([ 
            Svg.image
                [Svg.Attributes.width "100%", Svg.Attributes.height "100%", Svg.Attributes.xlinkHref "/pitch_h.png" ]
                []
        ] ++
            (case maybeEv of
                Nothing -> []
                Just ev -> [drawBall ev.ballPos]
            )
            {- show all positions. useful for debug
            ++
            (List.map drawBall <| List.concat <| List.map (\x -> List.map (\y -> (x,y)) [0,1,2,3,4,5,6]) [0,1,2,3,4])
            -}
        )

pitchX = 812
pitchY = 515
pitchPosPixelPos : (Int, Int) -> (Float, Float)
pitchPosPixelPos (x, y) =
    let
        xpadding = 100.0
        ypadding = 50.0
        xinc = (pitchX - 2*xpadding) / 6
        yinc = (pitchY - 2*ypadding) / 4
    in
        (xpadding + (toFloat (6-y))*xinc, ypadding + (toFloat x)*yinc)

    {- this worked for bigger pitch size -- may need when moving formations work
    let
        xpadding = 150.0
        ypadding = 50.0
        xinc = (pitchX - 2*xpadding) / 4
        yinc = (pitchY - 2*ypadding) / 4
    in
        (xpadding + (toFloat (4-y))*xinc, ypadding + (toFloat x)*yinc)
    -}

drawBall : (Int, Int) -> Svg.Svg Msg
drawBall (x, y) =
    let
        (xpos, ypos) = pitchPosPixelPos (x, y)
    in
        Svg.g [] [
            Svg.circle [ cx (toString xpos), cy (toString ypos), r "30", fill "white" ] []
            {-,
            Svg.text_ [fill "black", Svg.Attributes.x <| toString xpos, Svg.Attributes.y <| toString ypos]
                      [Svg.text ((toString x) ++ "," ++ (toString y))]
            -}
        ]


fixturesTable : Model -> Html Msg
fixturesTable model =
    let fixtureRow fixture =
            tr [Html.Attributes.class <|
                    if fixture.homeName == model.ourTeam.name || fixture.awayName == model.ourTeam.name then
                        "fixture-own" else "fixture-other",
                onClick (Watch fixture.gameId)] [
                td [] [text (fixture.homeName ++ " - " ++ fixture.awayName)],
                td [] [text <| Utils.timeFormat fixture.start],
                td [] [resultText fixture ]
            ]
        resultText fixture =
            case fixture.status of
                Scheduled -> text "Scheduled"
                InProgress -> text "In Progress!"
                Played result -> text (toString result.homeGoals ++ " : " ++ toString result.awayGoals)

        fixtureTable title fixtures = 
            div [] [
                h3 [] [text title],
                table [] ([
                        tr [] [
                            th [] [text "Game"],
                            th [] [text "Date"],
                            th [] [text "Result"]
                        ]
                    ] ++
                    List.map fixtureRow fixtures
                    )
                ]
    in
        div [] [
            case model.currentTime of
                Nothing -> span [] []
                Just now -> 
                    fixtureTable "Today's Matches"
                        (List.filter (\f -> Utils.dateEq (Date.fromTime f.start) (Date.fromTime now)) model.fixtures)
            ,
            fixtureTable "Season Fixtures" model.fixtures
        ]

update : Msg -> Model -> (Model, Cmd RootMsg.Msg)
update msg model =
    let
        latestEventId game = case List.head <| List.reverse game.events of
            Nothing -> Nothing
            Just event -> Just event.id
    in
        case msg of
            Watch gameId ->
                (model, Cmd.batch [ClientServer.loadGame gameId])
            ShowFinalScore -> case model.tab of
                TabFixtures (Just watchingGame) ->
                    ({ model | tab = TabFixtures (Just {
                         watchingGame | timePoint = watchingGame.timePoint + match_length_seconds*Time.second
                        })}, Cmd.none)
                _ -> (model, Cmd.none)
            GameTick -> case model.tab of
                TabFixtures (Just watchingGame) ->
                    let cmds = case List.head <| List.reverse watchingGame.game.events of
                        -- No game events yet. poll
                        Nothing -> Cmd.batch [ClientServer.pollGameEvents watchingGame.game.id Nothing]
                        Just e -> 
                            if e.kind == EndOfGame then
                                -- game ended. stop polling
                                Cmd.none
                            else
                                -- game in progress. get new game events
                                Cmd.batch [ClientServer.pollGameEvents watchingGame.game.id (Just e.id)]
                    in (
                        { model | tab = TabFixtures (Just { watchingGame | timePoint = watchingGame.timePoint + 1*Time.second})},
                        cmds
                    )
                _ -> (model, Cmd.none)
