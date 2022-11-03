module Internal.View exposing
    ( ViewConfigInternal
    , clearButtonElement
    , toElement
    , view
    )

import Browser.Dom as Dom
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Element.Region
import Html.Attributes
import Html.Events
import Internal.Filter as Filter exposing (Filter)
import Internal.Model as Model exposing (Model)
import Internal.Msg exposing (Msg(..))
import Internal.Option as Option exposing (Option)
import Internal.OptionState exposing (OptionState(..))
import Internal.Placement as Placement exposing (Placement)
import Internal.RequestState exposing (RequestState(..))
import Json.Decode as Decode


type alias ViewConfigInternal a msg =
    { onChange : Msg a -> msg
    , inputAttribs : List (Attribute msg)
    , itemToString : a -> String
    , label : Input.Label msg
    , placeholder : Maybe (Input.Placeholder msg)
    , filter : Maybe (Filter a)
    , menuPlacement : Maybe Placement
    , menuMaxHeight : Maybe Int
    , menuMaxWidth : Maybe Int
    , menuAttributes : List (Placement -> List (Attribute msg))
    , noMatchElement : Element msg
    , optionElement : OptionState -> a -> Element msg
    , clearButton : Maybe (Element msg)
    , positionFixed : Bool
    , clearInputValueOnBlur : Bool
    , selectExactMatchOnBlur : Bool
    }


view :
    List (Attribute msg)
    ->
        { onChange : Msg a -> msg
        , itemToString : a -> String
        , label : Input.Label msg
        , placeholder : Maybe (Input.Placeholder msg)
        }
    -> ViewConfigInternal a msg
view attribs v =
    { onChange = v.onChange
    , inputAttribs = attribs
    , itemToString = v.itemToString
    , optionElement = defaultOptionElement v.itemToString
    , label = v.label
    , placeholder = v.placeholder
    , filter = Just Filter.startsWithThenContains
    , menuPlacement = Nothing
    , menuMaxHeight = Nothing
    , menuMaxWidth = Nothing
    , menuAttributes = []
    , noMatchElement = defaultNoMatchElement
    , clearButton = Nothing
    , positionFixed = False
    , clearInputValueOnBlur = False
    , selectExactMatchOnBlur = False
    }


toElement : Model a -> ViewConfigInternal a msg -> Element msg
toElement model config =
    toElement_ (Model.toMenuPlacement config.menuMaxHeight config.menuPlacement model) (Model.toFilteredOptions config.itemToString config.filter model) model config


toElement_ : Placement -> List (Option a) -> Model a -> ViewConfigInternal a msg -> Element msg
toElement_ placement filteredOptions model config =
    Element.el
        ([ Element.htmlAttribute (Html.Attributes.id <| Model.toContainerElementId model)
         , Element.width Element.fill
         , Element.below <|
            if List.length filteredOptions == 0 && Model.isOpen model && not (String.isEmpty (Model.toInputValue model)) && (Model.toRequestState model == Nothing || Model.toRequestState model == Just Success) then
                config.noMatchElement

            else
                Element.none
         , Placement.toAttribute
            (if config.positionFixed then
                Placement.Below

             else
                placement
            )
           <|
            (if config.positionFixed then
                positionFixedEl placement (Model.toContainerElement model)

             else
                identity
            )
            <|
                menuView
                    (defaultMenuAttrs
                        { menuWidth = Model.toMenuMinWidth model
                        , maxWidth = config.menuMaxWidth
                        , menuHeight = Model.toMenuMaxHeight config.menuMaxHeight config.menuPlacement model
                        }
                        ++ List.concatMap (\toAttrs -> toAttrs (Model.toMenuPlacement config.menuMaxHeight config.menuPlacement model)) config.menuAttributes
                    )
                    { menuId = Model.toMenuElementId model
                    , toOptionId = Model.toOptionElementId model
                    , toOptionState = Model.toOptionState model
                    , onChange = config.onChange
                    , menuOpen = Model.isOpen model
                    , options = filteredOptions
                    , optionElement = config.optionElement
                    }
         ]
            ++ (if Model.isOpen model then
                    [ Element.htmlAttribute <| Html.Attributes.style "z-index" "21" ]

                else
                    []
               )
        )
        (inputView filteredOptions model config)


inputView : List (Option a) -> Model a -> ViewConfigInternal a msg -> Element msg
inputView filteredOptions model config =
    Input.text
        (config.inputAttribs
            ++ [ Events.onFocus (config.onChange InputFocused)
               , Events.onClick (config.onChange InputClicked)
               , Events.onLoseFocus
                    (config.onChange
                        (InputLostFocus
                            { clearInputValue = config.clearInputValueOnBlur
                            , selectExactMatch = config.selectExactMatchOnBlur
                            }
                            filteredOptions
                        )
                    )
               , onKeyDown (Model.isOpen model) (KeyDown filteredOptions >> config.onChange)
               , Element.htmlAttribute (Html.Attributes.id <| Model.toInputElementId model)
               , Element.inFront <|
                    if Model.toValue model /= Nothing || Model.toInputValue model /= "" then
                        Maybe.withDefault Element.none config.clearButton

                    else
                        Element.none
               ]
            ++ inputAccessibilityAttributes filteredOptions model
        )
        { onChange = InputChanged >> config.onChange
        , text =
            if Model.isFocused model then
                Model.toInputValue model

            else
                Maybe.andThen (Option.findByValue filteredOptions >> Maybe.map Option.toString) (Model.toValue model)
                    |> Maybe.withDefault (Model.toInputValue model)
        , placeholder = config.placeholder
        , label = config.label
        }


inputAccessibilityAttributes : List (Option a) -> Model a -> List (Attribute msg)
inputAccessibilityAttributes filteredOptions model =
    [ htmlAttribute "role" "combobox"
    , htmlAttribute "aria-owns" (Model.toMenuElementId model)
    , htmlAttribute "aria-autocomplete" "list"
    , htmlAttribute "aria-activedescendant" <|
        if Model.isOpen model then
            Model.toOptionElementId model (Model.toHighlighted model)

        else
            ""
    , htmlAttribute "aria-expanded"
        (if Model.isOpen model then
            "true"

         else
            "false"
        )
    , htmlAttribute "aria-haspopup" "listbox"
    , Element.below <|
        if Model.isOpen model then
            ariaLive (List.length filteredOptions)

        else
            Element.none
    ]


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
        |> Element.column (attribs ++ [ Element.htmlAttribute <| Html.Attributes.id v.menuId ])
        |> Element.el
            (Element.width Element.fill
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
clearButtonElement tagger attribs element =
    Input.button
        ([ Element.htmlAttribute (Html.Events.preventDefaultOn "mousedown" (Decode.succeed ( tagger NoOp, True )))
         , Element.htmlAttribute (Html.Events.preventDefaultOn "click" (Decode.succeed ( tagger ClearButtonPressed, True )))
         ]
            ++ attribs
        )
        { onPress = Just (tagger ClearButtonPressed)
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
    , Element.scrollbarY
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


onKeyDown : Bool -> (String -> msg) -> Attribute msg
onKeyDown menuOpen tagger =
    Element.htmlAttribute <|
        Html.Events.custom "keydown"
            (Decode.map (hijackKey menuOpen tagger)
                (Decode.field "key" Decode.string)
            )


hijackKey :
    Bool
    -> (String -> msg)
    -> String
    ->
        { message : msg
        , stopPropagation : Bool
        , preventDefault : Bool
        }
hijackKey menuOpen tagger key =
    { message = tagger key
    , stopPropagation = menuOpen && key == "Escape"
    , preventDefault = List.member key [ "ArrowUp", "ArrowDown", "PageUp", "PageDown" ]
    }


ariaLive : Int -> Element msg
ariaLive optionCount =
    Element.el
        [ Element.Region.announceUrgently
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
        (Element.text <|
            if optionCount > 0 then
                String.fromInt optionCount ++ " suggestions found. Use up and down arrows to review"

            else
                "No suggestions found."
        )


htmlAttribute : String -> String -> Attribute msg
htmlAttribute prop val =
    Element.htmlAttribute (Html.Attributes.attribute prop val)


style : String -> String -> Attribute msg
style prop val =
    Element.htmlAttribute (Html.Attributes.style prop val)
