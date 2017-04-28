module Model exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import Date exposing (Date)
import Debug
import Time exposing (Time)

-- MODEL

type alias WatchingGame = { gameId: GameId, timePoint: Time }

type UiTab = TabTeam | TabLeagueTables | TabFixtures (Maybe WatchingGame) | TabFinances

type alias RootModel = {
    errorMsg: Maybe String,
    state: RootState
}

type RootState = Loading | GameData Model

type alias Model = {
    ourTeamId: TeamId,
    tabTeamSelectedPlayer: Maybe Int,
    tab: UiTab,
    ourTeam : Team,
    fixtures: List Fixture,
    leagueTables : List LeagueTable,
    games: Dict GameId Game
}

type alias TeamId = Int
type alias Team = { id: TeamId, name: String, players: Array Player, formation: Array (Int, Int) }

type alias SeasonRecord = { teamId: TeamId, name: String, played: Int, won: Int, drawn: Int, lost: Int, goalsFor: Int, goalsAgainst: Int }
type alias LeagueTable = { name: String, record: List SeasonRecord }

{-
premierLeague : LeagueTable
premierLeague = { name="Scottish Premier Division", record=[
  { teamId=1, name="Rangers", played=1, won=0, drawn=0, lost=1, goalsFor=0, goalsAgainst=2 },
  { teamId=2, name="Celtic", played=1, won=1, drawn=0, lost=0, goalsFor=2, goalsAgainst=0 }
  ]}
-}

type alias PlayerId = Int
type alias Player = { id: PlayerId, name: String, skill: Int }

type alias GameId = Int
type GameEventType = Boring | HomeGoal | AwayGoal | EndOfGame
type alias GameEvent = { id: GameId, type_: GameEventType, timestamp: Time, message: String, ballPos: (Int, Int) }
type alias Game = { id: GameId, homeTeam: Team, awayTeam: Team, start: Time, events: List GameEvent }

type FixtureStatus = Scheduled | Played { homeGoals: Int, awayGoals: Int }
type alias Fixture = { gameId: GameId, homeName: String, awayName: String, start: Time, status: FixtureStatus }
