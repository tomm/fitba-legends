module TeamViewMsg exposing (Msg, Msg(..))

type Msg = SelectPlayer (Maybe Int) | MovePosition (Int, Int)
