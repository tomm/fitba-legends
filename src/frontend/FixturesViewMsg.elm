module FixturesViewMsg exposing (Msg, Msg(..))

import Types

type Msg = Watch Types.GameId | GameTick | ShowFinalScore
