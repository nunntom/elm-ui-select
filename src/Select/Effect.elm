module Select.Effect exposing (Effect, map, perform, performWith)

import Internal.Effect as Effect


type alias Effect eff msg =
    Effect.Effect eff msg


perform : Effect Never msg -> Cmd msg
perform =
    Effect.perform (\_ -> Cmd.none)


performWith : (eff -> Cmd msg) -> Effect eff msg -> Cmd msg
performWith =
    Effect.perform


map : (msg1 -> msg2) -> Effect eff msg1 -> Effect eff msg2
map =
    Effect.map
