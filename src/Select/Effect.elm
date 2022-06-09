module Select.Effect exposing (Effect, perform)

import Internal.Effect as Effect
import Msg exposing (Msg)


type alias Effect =
    Effect.Effect


perform : Effect -> Cmd (Msg a)
perform =
    Effect.perform
