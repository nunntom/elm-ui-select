module Select.Effect exposing (Effect, perform)

import Internal.Effect as Effect


type alias Effect msg =
    Effect.Effect msg


perform : Effect msg -> Cmd msg
perform =
    Effect.perform
