module Select.Effect exposing (Effect, map, perform)

import Internal.Effect as Effect


type alias Effect msg =
    Effect.Effect msg


perform : Effect msg -> Cmd msg
perform =
    Effect.perform


map : (msg1 -> msg2) -> Effect msg1 -> Effect msg2
map =
    Effect.map
