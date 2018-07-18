module TeamViewTypes exposing (..)
import Types

type alias SquadViewState = { selectedPlayer: Maybe Int }
type View = SquadView SquadViewState | PlayerView Types.Player
type alias State = { team: Types.Team, view: View }
type Msg = SellPlayer Types.Player | ViewSquad | ViewPlayer Types.Player | SelectPlayer (Maybe Int) | MovePosition (Int, Int)
