module Internal.View.Common exposing (ariaLive, inputAccessibilityAttributes, relativeContainerMarker)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Internal.Model as Model exposing (Model)


inputAccessibilityAttributes : Model a -> List (Attribute msg)
inputAccessibilityAttributes model =
    [ Html.Attributes.attribute "role" "combobox"
    , Html.Attributes.attribute "aria-owns" (Model.toMenuElementId model)
    , Html.Attributes.attribute "aria-autocomplete" "list"
    , Html.Attributes.attribute "aria-activedescendant" <|
        if Model.isOpen model then
            Model.toHighlighted model
                |> Maybe.map (Model.toOptionElementId model)
                |> Maybe.withDefault ""

        else
            ""
    , Html.Attributes.attribute "aria-expanded"
        (if Model.isOpen model then
            "true"

         else
            "false"
        )
    , Html.Attributes.attribute "aria-haspopup" "listbox"
    ]


ariaLive : Int -> Html msg
ariaLive optionCount =
    Html.div
        [ Html.Attributes.attribute "aria-live" "assertive"
        , style "position" "absolute"
        , style "width" "1px"
        , style "height" "1px"
        , style "padding" "0"
        , style "margin" "-1px"
        , style "overflow" "hidden"
        , style "clip" "rect(0, 0, 0, 0)"
        , style "white-space" "nowrap"
        , style "border" "0"
        , style "display" "hidden"
        ]
        [ Html.text <|
            if optionCount > 0 then
                String.fromInt optionCount ++ " suggestions found. Use up and down arrows to review"

            else
                "No suggestions found."
        ]


relativeContainerMarker : Model a -> Html msg
relativeContainerMarker model =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "height" "100%"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "width" "0"
        , Html.Attributes.style "visibility" "hidden"
        , Html.Attributes.id (Model.toRelativeContainerMarkerId model)
        ]
        []
