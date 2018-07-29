module RootMsg exposing (..)

import Http
import Time

import Model exposing (..)
import TeamViewTypes
import FixturesViewMsg
import TransferMarketTypes
import Types exposing (..)
import Notification

type Msg
  = ChangeTab UiTab | MsgTeamView TeamViewTypes.Msg
  | SecondTick Time.Time
  | MinuteTick Time.Time
  | MsgFixturesView FixturesViewMsg.Msg
  | MsgTransferMarket TransferMarketTypes.Msg
  | UpdateFixtures (Result Http.Error (List Fixture))
  | UpdateLeagueTables (Result Http.Error (List LeagueTable))
  | LoadGame (Result Http.Error Game)
  | UpdateGame (Result Http.Error (List GameEvent))
  | GotStartGameData (Result Http.Error Team)
  | SavedFormation (Result Http.Error String)
  | SavedBid (Result Http.Error String)
  | ViewTeam TeamId | ViewTeamLoaded (Result Http.Error Team)
  | ViewTransferMarket
  | GotTransferListings (Result Http.Error (List Types.TransferListing))
  | SellPlayerResponse (Result Http.Error String)
  | NoOp
  | RequestNotificationPermissionResult Notification.Permission
  | NotificationResult (Result Notification.Error ())
