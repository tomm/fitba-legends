module Model exposing (..)
import Array exposing (Array)
import Date exposing (Date)
import Debug
import Time exposing (Time)

import Types exposing (..)
import TransferMarketTypes
import TeamViewTypes

-- MODEL

type alias WatchingGame = { timePoint: Time, game: Game }
type alias TabTransferMarketState = { listings: List Player }

type UiTab = TabTeam TeamViewTypes.State |
             TabLeagueTables |
             TabFixtures (Maybe WatchingGame) |
             TabFinances |
             TabTransferMarket TransferMarketTypes.State |
             TabViewOtherTeam TeamViewTypes.State

type alias RootModel = {
    errorMsg: Maybe String,
    state: RootState
}

type RootState = Loading | GameData Model

type alias Model = {
    ourTeamId: TeamId,
    currentTime: Maybe Time.Time,
    tab: UiTab,
    ourTeam : Team,
    fixtures: List Fixture,
    leagueTables : List LeagueTable
}
