module Cmds exposing (saveFormation)

import Array
import Json.Decode
import Json.Encode
import Http

import Model exposing (Team)
import RootMsg exposing (Msg(SavedFormation))

saveFormation : Team -> Cmd Msg
saveFormation team = 
    let tuplePlayerPos idx player =
            case Array.get idx team.formation of
                Nothing -> Json.Encode.list [ Json.Encode.int player.id, Json.Encode.null ]
                Just pos -> Json.Encode.list [
                    Json.Encode.int player.id,
                    Json.Encode.list [ Json.Encode.int <| Tuple.first pos, Json.Encode.int <| Tuple.second pos ]
                    ]
        body = Http.jsonBody <| Json.Encode.array <| Array.indexedMap tuplePlayerPos team.players
    in
        Http.send SavedFormation (Http.post "/save_formation" body Json.Decode.string)
