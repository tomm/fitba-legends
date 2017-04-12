import Html exposing (Html, Attribute, div, input, text, ul, li, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Dict exposing (Dict)
import Array exposing (Array)
import Debug
import Maybe exposing (withDefault)
import Time


main =
    Html.program {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }


-- MODEL

type UiTab = TabTeam | TabLeagueTables | TabFixtures | TabFinances

type alias Model = { ourTeamId: TeamId, tabTeamSelectedPlayer: Maybe Int, tab: UiTab, teams : Dict Int Team }
type alias TeamId = Int
type alias Team = { id: TeamId, name: String, players: Array Player }

type alias SeasonRecord = { teamId: TeamId, played: Int, won: Int, drawn: Int, lost: Int, goalsFor: Int, goalsAgainst: Int }
type alias LeagueTable = { name: String, record: List SeasonRecord }

premierLeague : LeagueTable
premierLeague = { name="Scottish Premier Division", record=[
  { teamId=1, played=1, won=0, drawn=0, lost=1, goalsFor=0, goalsAgainst=2 },
  { teamId=2, played=1, won=1, drawn=0, lost=0, goalsFor=2, goalsAgainst=0 }
  ]}

type alias Player = { name: String, skill: Int }

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
  = ChangeTab UiTab | SelectPlayer (Maybe Int) | ClockTick Time.Time

ourTeam : Model -> Team
ourTeam model = case Dict.get model.ourTeamId model.teams of
            Just t -> t
            Nothing -> Debug.crash("Error! model.ourTeamId is invalid!")

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeTab tab -> ({ model | tab = tab }, Cmd.none)
    SelectPlayer (Just p) -> (applySelectPlayer model p, Cmd.none)
    SelectPlayer Nothing -> ({ model | tabTeamSelectedPlayer = Nothing }, Cmd.none)
    ClockTick t -> (model, Cmd.none)

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

-- VIEW

activeTabStyle : Attribute Msg
activeTabStyle = style [("border", "none"), ("width", "100%"), ("height", "3em"), ("backgroundColor", "blue"), ("color", "white")]

inactiveTabStyle : Attribute Msg
inactiveTabStyle = style [("border", "none"), ("width", "100%"), ("height", "3em"), ("backgroundColor", "white")]

activeTableRowStyle : Attribute Msg
activeTableRowStyle = style [("backgroundColor", "blue")]

tableStyle : Attribute Msg
tableStyle = style [("width", "100%"), ("borderCollapse", "collapse")]

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
                     Just team -> teamTab model team
                     Nothing -> text "Error: unknown teamId"
          TabLeagueTables -> leagueTableTab model premierLeague
          _ -> text "Arse"
      ]
    ]

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
