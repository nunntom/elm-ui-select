module Internal.View.ElmCss exposing
    ( Config
    , ViewConfig
    , defaultOptionElement
    , init
    , toStyled
    )

import Browser.Dom as Dom
import Css exposing (Style)
import Element exposing (Attribute)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Internal.Model as Model exposing (Model)
import Internal.Msg exposing (Msg(..))
import Internal.Option as Option exposing (Option)
import Internal.OptionState exposing (OptionState(..))
import Internal.Placement as Placement exposing (Placement)
import Internal.View.Common as View
import Internal.View.Events as ViewEvents
import Internal.ViewConfig as ViewConfig exposing (ViewConfigInternal)
import Json.Decode as Decode


type alias Config a msg =
    { select : Model a
    , onChange : Msg a -> msg
    , itemToString : a -> String
    }


type alias ViewConfig a msg =
    ViewConfigInternal a Style (Html msg)


init : ViewConfig a msg
init =
    ViewConfig.init


toStyled : List Style -> Config a msg -> ViewConfig a msg -> Html msg
toStyled attrs ({ select } as config) viewConfig =
    toStyled_ attrs
        (ViewConfig.toPlacement select viewConfig)
        (ViewConfig.toFilteredOptions select config.itemToString viewConfig)
        config
        viewConfig


toStyled_ : List Style -> Placement -> List (Option a) -> Config a msg -> ViewConfig a msg -> Html msg
toStyled_ attrs placement filteredOptions ({ select } as config) viewConfig =
    Html.div
        (List.concat
            [ [ Attributes.id <| Model.toContainerElementId select
              , Attributes.class "elm-select-container"
              , Attributes.css
                    [ Css.position Css.relative
                    , Css.boxSizing Css.borderBox
                    , if Model.isOpen select then
                        Css.zIndex (Css.int 21)

                      else
                        Css.batch []
                    ]
              ]
            , ViewEvents.updateFilteredOptions config.onChange config.itemToString select viewConfig filteredOptions
                |> List.map Attributes.fromUnstyled
            ]
        )
        [ View.relativeContainerMarker select
            |> Html.fromUnstyled
        , inputView attrs filteredOptions config viewConfig
        , if Model.toValue select /= Nothing || Model.toInputValue select /= "" then
            viewConfig.clearButton
                |> Maybe.map
                    (\( attrs_, el ) ->
                        clearButtonElement config.onChange attrs_ el
                    )
                |> Maybe.withDefault (Html.text "")

          else
            Html.text ""
        , if ViewConfig.shouldShowNoMatchElement filteredOptions select viewConfig then
            Html.div
                [ Attributes.css
                    [ Css.position Css.absolute
                    , Css.width (Css.pct 100)
                    ]
                ]
                [ Maybe.withDefault defaultNoMatchElement viewConfig.noMatchElement ]

          else
            Html.text ""
        , if viewConfig.positionFixed then
            positionFixedEl placement
                (Model.toContainerElement select)
                (menuView
                    (defaultMenuAttrs placement
                        (List.concatMap (\toAttrs -> toAttrs placement) viewConfig.menuAttributes)
                        { menuWidth = Model.toMenuMinWidth select
                        , maxWidth = viewConfig.menuMaxWidth
                        , menuHeight = Model.toMenuMaxHeight viewConfig.menuMaxHeight viewConfig.menuPlacement select
                        }
                    )
                    { menuId = Model.toMenuElementId select
                    , toOptionId = Model.toOptionElementId select
                    , toOptionState = Model.toOptionState select
                    , onChange = config.onChange
                    , menuOpen = Model.isOpen select
                    , options = filteredOptions
                    , optionElement = Maybe.withDefault (defaultOptionElement config.itemToString) viewConfig.optionElement
                    }
                )

          else
            menuView
                (defaultMenuAttrs placement
                    (List.concatMap (\toAttrs -> toAttrs placement) viewConfig.menuAttributes)
                    { menuWidth = Model.toMenuMinWidth select
                    , maxWidth = viewConfig.menuMaxWidth
                    , menuHeight = Model.toMenuMaxHeight viewConfig.menuMaxHeight viewConfig.menuPlacement select
                    }
                )
                { menuId = Model.toMenuElementId select
                , toOptionId = Model.toOptionElementId select
                , toOptionState = Model.toOptionState select
                , onChange = config.onChange
                , menuOpen = Model.isOpen select
                , options = filteredOptions
                , optionElement = Maybe.withDefault (defaultOptionElement config.itemToString) viewConfig.optionElement
                }
        , if Model.isOpen select then
            View.ariaLive (List.length filteredOptions)
                |> Html.fromUnstyled

          else
            Html.text ""
        ]


inputView : List Style -> List (Option a) -> Config a msg -> ViewConfig a msg -> Html msg
inputView attrs filteredOptions ({ select } as config) viewConfig =
    Html.input
        ([ ViewEvents.onFocus config.onChange config.itemToString select viewConfig filteredOptions
            |> Attributes.fromUnstyled
         , Events.onClick (InputClicked |> config.onChange)
         , Events.onBlur
            (config.onChange
                (InputLostFocus
                    { clearInputValue = viewConfig.clearInputValueOnBlur
                    , selectExactMatch = viewConfig.selectExactMatchOnBlur
                    }
                    filteredOptions
                )
            )
         , Attributes.fromUnstyled <|
            ViewEvents.onKeyDown (Model.isOpen select) (KeyDown viewConfig.selectOnTab filteredOptions >> config.onChange)
         , Attributes.id <| Model.toInputElementId select
         , Events.onInput (ViewEvents.onInput config.onChange config.itemToString select viewConfig)
         , Attributes.value <| Model.toInputText config.itemToString select
         , Attributes.attribute "autocomplete" "dont-fill-in-this-box"
         , Attributes.css
            [ Css.width (Css.pct 100)
            , Css.boxSizing Css.borderBox
            , Css.batch attrs
            ]
         ]
            ++ List.map Attributes.fromUnstyled (View.inputAccessibilityAttributes select)
        )
        []


menuView :
    List (Attribute msg)
    ->
        { menuId : String
        , toOptionId : Int -> String
        , toOptionState : ( Int, a ) -> OptionState
        , onChange : Msg a -> msg
        , menuOpen : Bool
        , options : List (Option a)
        , optionElement : OptionState -> a -> Html msg
        }
    -> Html msg
menuView attribs v =
    List.indexedMap (optionElement v) v.options
        |> Html.div
            (attribs
                ++ (Attributes.id v.menuId
                        :: (if v.menuOpen && List.length v.options > 0 then
                                []

                            else
                                [ Attributes.style "visibility" "hidden"
                                , Attributes.attribute "aria-visible"
                                    (if v.menuOpen then
                                        "false"

                                     else
                                        "true"
                                    )
                                , Attributes.style "height" "0"
                                , Attributes.style "overflow-y" "hidden"
                                ]
                           )
                   )
            )


optionElement :
    { b
        | toOptionState : ( Int, a ) -> OptionState
        , toOptionId : Int -> String
        , onChange : Msg a -> msg
        , optionElement : OptionState -> a -> Html msg
    }
    -> Int
    -> Option a
    -> Html msg
optionElement v i opt =
    let
        optionState =
            v.toOptionState ( i, Option.toItem opt )
    in
    Html.div
        ([ Attributes.id (v.toOptionId i)
         , Attributes.attribute "role" "option"
         , Attributes.attribute "value" (Option.toString opt)
         , Events.preventDefaultOn "mousedown" (Decode.succeed ( v.onChange NoOp, True ))
         , Events.preventDefaultOn "click" (Decode.succeed ( v.onChange <| OptionClicked opt, True ))
         ]
            ++ (if optionState /= Highlighted then
                    [ Events.on "mousemove" (Decode.succeed (v.onChange <| MouseEnteredOption i)) ]

                else
                    []
               )
        )
        [ v.optionElement optionState (Option.toItem opt) ]


clearButtonElement : (Msg a -> msg) -> List Style -> Html msg -> Html msg
clearButtonElement onChange attribs element =
    Html.button
        [ Attributes.css
            [ Css.position Css.absolute
            , Css.right Css.zero
            , Css.top Css.zero
            , Css.backgroundColor Css.transparent
            , Css.borderWidth Css.zero
            , Css.padding Css.zero
            , Css.margin Css.zero
            , Css.batch attribs
            ]
        , Attributes.tabindex -1
        , Attributes.type_ "button"
        , Attributes.attribute "aria-label" "clear"
        , Events.onClick (onChange ClearButtonPressed)
        ]
        [ element ]


defaultMenuAttrs :
    Placement
    -> List Style
    ->
        { menuWidth : Maybe Int
        , maxWidth : Maybe Int
        , menuHeight : Maybe Int
        }
    -> List (Attribute msg)
defaultMenuAttrs placement css { menuWidth, maxWidth, menuHeight } =
    [ Attributes.attribute "role" "listbox"
    , Attributes.css
        [ Css.position Css.absolute
        , case placement of
            Placement.Above ->
                Css.bottom (Css.pct 100)

            Placement.Below ->
                Css.batch []
        , Maybe.map (toFloat >> Css.px >> Css.maxHeight) menuHeight
            |> Maybe.withDefault (Css.batch [])
        , Maybe.map (toFloat >> Css.px >> Css.maxWidth) maxWidth
            |> Maybe.withDefault (Css.batch [])
        , Maybe.map (toFloat >> Css.px >> Css.minWidth) menuWidth
            |> Maybe.withDefault (Css.batch [])
        , Css.overflowY Css.scroll
        , Css.border3 (Css.px 1) Css.solid (Css.rgb 204 204 204)
        , Css.borderRadius (Css.px 5)
        , Css.backgroundColor (Css.rgb 255 255 255)
        , Css.padding2 (Css.px 5) (Css.px 0)
        , Css.width (Css.pct 100)
        , Css.boxSizing Css.borderBox
        , Css.batch css
        ]
    ]


positionFixedEl : Placement -> Maybe Dom.Element -> Html msg -> Html msg
positionFixedEl placement container content =
    Html.div
        (Attributes.style "position" "fixed"
            :: (if placement == Placement.Above then
                    [ Attributes.style "transform"
                        ("translateY(calc(-100% - 5px - "
                            ++ (Maybe.map (.element >> .height >> String.fromFloat) container |> Maybe.withDefault "0")
                            ++ "px))"
                        )
                    ]

                else
                    []
               )
        )
        [ content ]


defaultOptionElement : (a -> String) -> OptionState -> a -> Html msg
defaultOptionElement toString optionState a =
    Html.div
        [ Attributes.style "cursor" "pointer"
        , Attributes.style "padding" "10px 14px"
        , Attributes.style "background-color" <|
            case optionState of
                Highlighted ->
                    "rgb(89%, 89%, 89%)"

                Selected ->
                    "rgba(64%, 83%, 97%, 0.8)"

                SelectedAndHighlighted ->
                    "rgba(64%, 83%, 97%, 1)"

                Idle ->
                    "rgb(255, 255, 255)"
        ]
        [ Html.text (toString a) ]


defaultNoMatchElement : Html msg
defaultNoMatchElement =
    Html.div
        [ Attributes.css
            [ Css.padding (Css.px 5)
            , Css.border3 (Css.px 1) Css.solid (Css.rgba 0 0 0 0.5)
            , Css.borderRadius (Css.px 5)
            , Css.backgroundColor (Css.rgb 255 255 255)
            , Css.width (Css.pct 100)
            ]
        ]
        [ Html.text "No matches" ]
