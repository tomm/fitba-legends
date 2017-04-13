module Model exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import Date exposing (Date)
import Debug

-- MODEL

type UiTab = TabTeam | TabLeagueTables | TabFixtures | TabFinances

type alias Model = { ourTeamId: TeamId, tabTeamSelectedPlayer: Maybe Int, tab: UiTab, ourTeam : Team }
type alias TeamId = Int
type alias Team = { id: TeamId, name: String, players: Array Player, formation: Array (Int, Int) }

type alias SeasonRecord = { teamId: TeamId, name: String, played: Int, won: Int, drawn: Int, lost: Int, goalsFor: Int, goalsAgainst: Int }
type alias LeagueTable = { name: String, record: List SeasonRecord }

premierLeague : LeagueTable
premierLeague = { name="Scottish Premier Division", record=[
  { teamId=1, name="Rangers", played=1, won=0, drawn=0, lost=1, goalsFor=0, goalsAgainst=2 },
  { teamId=2, name="Celtic", played=1, won=1, drawn=0, lost=0, goalsFor=2, goalsAgainst=0 }
  ]}

type alias Player = { name: String, skill: Int }

----type GameEventType = Boring | HomeGoal | AwayGoal;
----type alias GameEvent = { type_: GameEventType, timestamp: Date, message: String, ballPos: (Int, Int) }
----type alias GameId = Int
----type alias Game = { id: GameId, homeTeam: Team, awayTeam: Team, events: List GameEvent }
