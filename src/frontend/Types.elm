module Types exposing (..)
import Round

import Array exposing (Array)
import Time exposing (Time)

type alias TeamId = Int
type alias Team = { id: TeamId, name: String, players: Array Player, formation: Array (Int, Int), money: Maybe Int }
type alias SeasonRecord = { teamId: TeamId, name: String, played: Int, won: Int, drawn: Int, lost: Int, goalsFor: Int, goalsAgainst: Int }
type alias LeagueTable = { name: String, record: List SeasonRecord }
type alias PlayerId = Int
type alias Player = { id: PlayerId, name: String, shooting: Int, passing: Int, tackling: Int,
                      handling: Int, speed: Int, positions: List (Int, Int) }
type alias TransferListingId = Int
type TransferStatus = OnSale | YouWon | YouLost | Sold | Unsold
type alias TransferListing = { id: TransferListingId, minPrice: Int, deadline: Time, sellerTeamId: TeamId,
                               player: Player, youBid: Maybe Int, status: TransferStatus }
type alias GameId = Int
type alias GameEventId = Int
type GameEventKind = KickOff | Goal | Boring | ShotTry | ShotMiss | ShotSaved | EndOfGame
type GameEventSide = Home | Away
type alias GameEvent = { id: GameEventId, gameId: GameId, kind: GameEventKind, side: GameEventSide,
                         timestamp: Time, message: String, ballPos: (Int, Int), playerName: Maybe String }
type alias Game = { id: GameId, homeTeam: Team, awayTeam: Team, start: Time, events: List GameEvent, status: FixtureStatus }
type alias FixtureStatusPlayed = { homeGoals: Int, awayGoals: Int }
type FixtureStatus = Scheduled | InProgress | Played FixtureStatusPlayed
type alias Fixture = { gameId: GameId, homeName: String, awayName: String, start: Time, status: FixtureStatus }

playerPositionFormat : List (Int, Int) -> String
playerPositionFormat ps =
    if List.member (2,6) ps then "GK"
    else if List.member (2,5) ps then "DC"
    else if List.member (2,4) ps then "DMC"
    else if List.member (2,3) ps then "MC"
    else if List.member (2,2) ps then "AMC"
    else if List.member (2,1) ps then "CF"
    else if List.member (0,5) ps then "DL"
    else if List.member (4,5) ps then "DR"
    else if List.member (0,4) ps then "DML"
    else if List.member (4,4) ps then "DMR"
    else if List.member (0,3) ps then "ML"
    else if List.member (4,3) ps then "MR"
    else if List.member (0,2) ps then "AML"
    else if List.member (4,2) ps then "AMR"
    else "ERROR: " ++ (toString ps)

playerAvgSkill : Player -> String
playerAvgSkill p = Round.round 1 <| 0.2 * toFloat (p.shooting + p.passing + p.tackling + p.handling + p.speed)
