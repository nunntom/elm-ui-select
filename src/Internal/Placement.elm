module Internal.Placement exposing (Placement(..), toAttribute)

import Element exposing (Attribute, Element)


type Placement
    = Above
    | Below


toAttribute : Maybe Placement -> (Element msg -> Attribute msg)
toAttribute placement =
    case placement of
        Just Above ->
            Element.above

        _ ->
            Element.below
