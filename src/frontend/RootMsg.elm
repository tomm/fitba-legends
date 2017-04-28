module RootMsg exposing (..)

import Http
import Time

import Model exposing (..)
import TeamViewMsg
import FixturesViewMsg

type Msg
  = ChangeTab UiTab | MsgTeamView TeamViewMsg.Msg | ClockTick Time.Time
  | MsgFixturesView FixturesViewMsg.Msg
  | UpdateFixtures (Result Http.Error (List Fixture))
  | UpdateLeagueTables (Result Http.Error (List LeagueTable))
  | GotStartGameData (Result Http.Error Team)
  | SavedFormation (Result Http.Error String)
