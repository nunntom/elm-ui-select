module Internal.Placement exposing (Placement(..), toAttribute)

import Ui


type Placement
    = Above
    | Below


toAttribute : Placement -> (Ui.Element msg -> Ui.Attribute msg)
toAttribute placement =
    case placement of
        Above ->
            Ui.above

        Below ->
            Ui.below
