module Internal.Placement exposing (Placement(..), toAttribute)

import Element exposing (Attribute, Element)


type Placement
    = Above
    | Below


toAttribute : Placement -> (Element msg -> Attribute msg)
toAttribute placement =
    case placement of
        Above ->
            Element.above

        Below ->
            Element.below
