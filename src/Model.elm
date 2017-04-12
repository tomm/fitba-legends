module Model exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import Debug

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

ourTeam : Model -> Team
ourTeam model = case Dict.get model.ourTeamId model.teams of
            Just t -> t
            Nothing -> Debug.crash("Error! model.ourTeamId is invalid!")
