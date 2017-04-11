import Html exposing (Html, Attribute, div, input, text, ul, li, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Dict exposing (Dict)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type UiTab = TabTeam | TabLeagueTables | TabFixtures | TabFinances

type alias Model = { tab: UiTab, content : String, teams : Dict Int Team }

model : Model
model =
  { content = "", tab = TabTeam, teams = teams }

type alias TeamId = Int
type alias Team = { id: TeamId, name: String }

teams : Dict TeamId Team
teams = Dict.fromList[
  (1, {id=1, name="Rangers"}),
  (2, {id=2, name="Celtic"})
  ]

type alias SeasonRecord = { teamId: TeamId, played: Int, won: Int, drawn: Int, lost: Int, goalsFor: Int, goalsAgainst: Int }
type alias LeagueTable = { name: String, record: List SeasonRecord }

premierLeague : LeagueTable
premierLeague = { name="Scottish Premier Division", record=[
  { teamId=1, played=1, won=0, drawn=0, lost=1, goalsFor=0, goalsAgainst=2 },
  { teamId=2, played=1, won=1, drawn=0, lost=0, goalsFor=2, goalsAgainst=0 }
  ]}

-- UPDATE

type Msg
  = Change String | ChangeTab UiTab

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }
    ChangeTab tab -> { model | tab = tab }

-- VIEW

activeTabStyle : Attribute Msg
activeTabStyle = style [("border", "none"), ("width", "10em"), ("height", "2em"), ("backgroundColor", "blue"), ("color", "white")]

inactiveTabStyle : Attribute Msg
inactiveTabStyle = style [("border", "none"), ("width", "10em"), ("height", "2em"), ("backgroundColor", "white")]

tabs : Model -> Html Msg
tabs model =
  let liStyle = style[("display", "block"), ("float", "left")]
      tabStyle tab = if model.tab == tab then activeTabStyle else inactiveTabStyle
      tabLabels = [(TabTeam, "Team"), (TabLeagueTables, "Tables"), (TabFixtures, "Fixtures"), (TabFinances, "Finances")]

  in ul [style [("listStyleType", "none")]]
      (List.map (\(tab, label) ->
          li [liStyle] [button [onClick (ChangeTab tab), tabStyle tab] [text label]]
        )
        tabLabels)

view : Model -> Html Msg
view model =
  div []
    [ div [] [tabs model]
    , div [style [("clear", "both")]] [
        case model.tab of
          TabTeam -> teamTab model
          TabLeagueTables -> leagueTableTab premierLeague
          _ -> text "Arse"
      ]
    ]

teamTab : Model -> Html Msg
teamTab model = div [style [("clear", "both")]] [
  input [ placeholder "Text to reverse", onInput Change ] []
  , div [] [ text (String.reverse model.content) ]]

leagueTableTab : LeagueTable -> Html Msg
leagueTableTab league =
  let recordToTableLine record =
    Html.tr [] (case Dict.get record.teamId teams of
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
      Html.table [style [("width", "100%")]] (
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
