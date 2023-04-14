module Internal.View.ElmUi exposing
    ( Config
    , ViewConfig
    , defaultOptionElement
    , init
    , toElement
    )

import Browser.Dom as Dom
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html.Attributes
import Html.Events
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
    , label : Input.Label msg
    , placeholder : Maybe (Input.Placeholder msg)
    }


type alias ViewConfig a msg =
    ViewConfigInternal a (Attribute msg) (Element msg)


init : ViewConfigInternal a (Attribute msg) (Element msg)
init =
    ViewConfig.init


toElement : List (Attribute msg) -> Config a msg -> ViewConfig a msg -> Element msg
toElement attrs ({ select } as config) viewConfig =
    toElement_ attrs
        (ViewConfig.toPlacement select viewConfig)
        (ViewConfig.toFilteredOptions select config.itemToString viewConfig)
        config
        viewConfig


toElement_ : List (Attribute msg) -> Placement -> List (Option a) -> Config a msg -> ViewConfig a msg -> Element msg
toElement_ attrs placement filteredOptions ({ select } as config) viewConfig =
    Element.el
        (List.concat
            [ [ Element.htmlAttribute (Html.Attributes.id <| Model.toContainerElementId select)
              , Element.htmlAttribute (Html.Attributes.class "elm-select-container")
              , View.relativeContainerMarker select
                    |> Element.html
                    |> Element.inFront
              , Element.width Element.fill
              , Element.below <|
                    if ViewConfig.shouldShowNoMatchElement filteredOptions select viewConfig then
                        Maybe.withDefault defaultNoMatchElement viewConfig.noMatchElement

                    else
                        Element.none
              , Placement.toAttribute
                    (if viewConfig.positionFixed then
                        Placement.Below

                     else
                        placement
                    )
                <|
                    (if viewConfig.positionFixed then
                        positionFixedEl placement (Model.toContainerElement select)

                     else
                        identity
                    )
                    <|
                        menuView
                            (defaultMenuAttrs
                                { menuWidth = Model.toMenuMinWidth select
                                , maxWidth = viewConfig.menuMaxWidth
                                , menuHeight = Model.toMenuMaxHeight viewConfig.menuMaxHeight viewConfig.menuPlacement select
                                }
                                ++ List.concatMap (\toAttrs -> toAttrs placement) viewConfig.menuAttributes
                            )
                            { menuId = Model.toMenuElementId select
                            , toOptionId = Model.toOptionElementId select
                            , toOptionState = Model.toOptionState select
                            , onChange = config.onChange
                            , menuOpen = Model.isOpen select
                            , options = filteredOptions
                            , optionElement = Maybe.withDefault (defaultOptionElement config.itemToString) viewConfig.optionElement
                            }
              ]
            , if Model.isOpen select then
                [ Element.htmlAttribute <| Html.Attributes.style "z-index" "21" ]

              else
                []
            , ViewEvents.updateFilteredOptions config.onChange config.itemToString select viewConfig filteredOptions
                |> List.map Element.htmlAttribute
            ]
        )
        (inputView attrs filteredOptions config viewConfig)


inputView : List (Attribute msg) -> List (Option a) -> Config a msg -> ViewConfig a msg -> Element msg
inputView attrs filteredOptions ({ select } as config) viewConfig =
    Input.text
        (List.concat
            [ attrs
            , [ ViewEvents.onFocus config.onChange config.itemToString select viewConfig filteredOptions
                    |> Element.htmlAttribute
              , Events.onClick (InputClicked |> config.onChange)
              , Events.onLoseFocus
                    (config.onChange
                        (InputLostFocus
                            { clearInputValue = viewConfig.clearInputValueOnBlur
                            , selectExactMatch = viewConfig.selectExactMatchOnBlur
                            }
                            filteredOptions
                        )
                    )
              , Element.htmlAttribute <|
                    ViewEvents.onKeyDown (Model.isOpen select) (KeyDown viewConfig.selectOnTab filteredOptions >> config.onChange)
              , Element.htmlAttribute (Html.Attributes.id <| Model.toInputElementId select)
              , Element.inFront <|
                    if Model.toValue select /= Nothing || Model.toInputValue select /= "" then
                        viewConfig.clearButton
                            |> Maybe.map (\( attrs_, el ) -> clearButtonElement config.onChange attrs_ el)
                            |> Maybe.withDefault Element.none

                    else
                        Element.none
              ]
            , List.map Element.htmlAttribute (View.inputAccessibilityAttributes select)
            , [ Element.below <|
                    if Model.isOpen select then
                        Element.html <| View.ariaLive (List.length filteredOptions)

                    else
                        Element.none
              ]
            ]
        )
        { onChange = ViewEvents.onInput config.onChange config.itemToString select viewConfig
        , text = Model.toInputText config.itemToString select
        , placeholder = config.placeholder
        , label = config.label
        }


menuView :
    List (Attribute msg)
    ->
        { menuId : String
        , toOptionId : Int -> String
        , toOptionState : ( Int, a ) -> OptionState
        , onChange : Msg a -> msg
        , menuOpen : Bool
        , options : List (Option a)
        , optionElement : OptionState -> a -> Element msg
        }
    -> Element msg
menuView attribs v =
    List.indexedMap (optionElement v) v.options
        |> Element.column
            (attribs
                ++ (Element.htmlAttribute <| Html.Attributes.id v.menuId)
                :: (if v.menuOpen && List.length v.options > 0 then
                        []

                    else
                        [ style "visibility" "hidden"
                        , htmlAttribute "aria-visible"
                            (if v.menuOpen then
                                "false"

                             else
                                "true"
                            )
                        , Element.height (Element.px 0)
                        , Element.clipY
                        ]
                   )
            )


optionElement :
    { b
        | toOptionState : ( Int, a ) -> OptionState
        , toOptionId : Int -> String
        , onChange : Msg a -> msg
        , optionElement : OptionState -> a -> Element msg
    }
    -> Int
    -> Option a
    -> Element msg
optionElement v i opt =
    Element.row
        [ Element.htmlAttribute (Html.Attributes.id (v.toOptionId i))
        , htmlAttribute "role" "option"
        , htmlAttribute "value" (Option.toString opt)
        , Element.htmlAttribute (Html.Events.preventDefaultOn "mousedown" (Decode.succeed ( v.onChange NoOp, True )))
        , Element.htmlAttribute (Html.Events.preventDefaultOn "click" (Decode.succeed ( v.onChange <| OptionClicked opt, True )))
        , Events.onMouseEnter (v.onChange <| MouseEnteredOption i)
        , Element.width Element.fill
        ]
        [ v.optionElement (v.toOptionState ( i, Option.toItem opt )) (Option.toItem opt) ]


clearButtonElement : (Msg a -> msg) -> List (Attribute msg) -> Element msg -> Element msg
clearButtonElement onChange attribs element =
    Input.button
        (attribs
            ++ [ Element.htmlAttribute <|
                    Html.Events.preventDefaultOn "click" (Decode.succeed ( onChange ClearButtonPressed, True ))
               ]
        )
        { onPress = Just (onChange ClearButtonPressed)
        , label = element
        }


defaultMenuAttrs :
    { menuWidth : Maybe Int
    , maxWidth : Maybe Int
    , menuHeight : Maybe Int
    }
    -> List (Attribute msg)
defaultMenuAttrs { menuWidth, maxWidth, menuHeight } =
    [ Element.shrink
        |> (Maybe.map Element.maximum menuHeight |> Maybe.withDefault identity)
        |> Element.height
    , Element.width <|
        case menuWidth of
            Just w ->
                Element.shrink
                    |> Element.minimum w
                    |> (Maybe.map Element.maximum maxWidth |> Maybe.withDefault identity)

            Nothing ->
                Element.fill

    --, Element.scrollbarY
    , style "overflow-y" "auto"
    , Border.solid
    , Border.color (Element.rgb 0.8 0.8 0.8)
    , Border.width 1
    , Border.rounded 5
    , Background.color (Element.rgb 1 1 1)
    , Element.paddingXY 0 5
    , htmlAttribute "role" "listbox"
    ]


positionFixedEl : Placement -> Maybe Dom.Element -> Element msg -> Element msg
positionFixedEl placement container =
    Element.el
        (style "position" "fixed"
            :: (if placement == Placement.Above then
                    [ style "transform"
                        ("translateY(calc(-100% - 5px - "
                            ++ (Maybe.map (.element >> .height >> String.fromFloat) container |> Maybe.withDefault "0")
                            ++ "px))"
                        )
                    ]

                else
                    []
               )
        )


defaultOptionElement : (a -> String) -> OptionState -> a -> Element msg
defaultOptionElement toString optionState a =
    Element.el
        [ Element.width Element.fill
        , Element.pointer
        , Element.paddingXY 14 10
        , Background.color <|
            case optionState of
                Highlighted ->
                    Element.rgb 0.89 0.89 0.89

                Selected ->
                    Element.rgba 0.64 0.83 0.97 0.8

                SelectedAndHighlighted ->
                    Element.rgba 0.64 0.83 0.97 1

                Idle ->
                    Element.rgb 1 1 1
        ]
        (Element.text (toString a))


defaultNoMatchElement : Element msg
defaultNoMatchElement =
    Element.el
        [ Element.padding 5
        , Border.solid
        , Border.color (Element.rgb 0.8 0.8 0.8)
        , Border.width 1
        , Border.rounded 5
        , Background.color (Element.rgb 1 1 1)
        , Element.width Element.fill
        ]
        (Element.text "No matches")


htmlAttribute : String -> String -> Attribute msg
htmlAttribute prop val =
    Element.htmlAttribute (Html.Attributes.attribute prop val)


style : String -> String -> Attribute msg
style prop val =
    Element.htmlAttribute (Html.Attributes.style prop val)
