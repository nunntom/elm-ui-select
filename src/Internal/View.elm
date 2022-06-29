module Internal.View exposing
    ( ClearButton
    , OptionState(..)
    , ViewConfig
    , ViewConfigInternal
    , clearButton
    , clearButtonElement
    , toElement
    , view
    , withClearButton
    , withFilter
    , withMenuAlwaysAbove
    , withMenuAlwaysBelow
    , withMenuAttributes
    , withMenuMaxHeight
    , withMenuMaxWidth
    , withNoMatchElement
    , withOptionElement
    )

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Html.Events
import Internal.Filter as Filter exposing (Filter)
import Internal.Model as Model exposing (Model)
import Internal.Msg exposing (Msg(..))
import Internal.Option exposing (Option)
import Internal.Placement as Placement exposing (Placement(..))
import Internal.RequestState exposing (RequestState(..))
import Json.Decode as Decode


type ViewConfig a msg
    = ViewConfig (ViewConfigInternal a msg)


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
    , menuAttributes : List (Attribute msg)
    , noMatchElement : Element msg
    , optionElement : OptionState -> a -> Element msg
    , clearButton : Maybe (Element msg)
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
    }


withFilter : Filter a -> ViewConfig a msg -> ViewConfig a msg
withFilter filter (ViewConfig config) =
    ViewConfig { config | filter = Just filter }


withMenuAlwaysAbove : ViewConfig a msg -> ViewConfig a msg
withMenuAlwaysAbove (ViewConfig config) =
    ViewConfig { config | menuPlacement = Just Above }


withMenuAlwaysBelow : ViewConfig a msg -> ViewConfig a msg
withMenuAlwaysBelow (ViewConfig config) =
    ViewConfig { config | menuPlacement = Just Below }


withMenuMaxHeight : Int -> ViewConfig a msg -> ViewConfig a msg
withMenuMaxHeight height (ViewConfig config) =
    ViewConfig { config | menuMaxHeight = Just height }


withMenuMaxWidth : Int -> ViewConfig a msg -> ViewConfig a msg
withMenuMaxWidth height (ViewConfig config) =
    ViewConfig { config | menuMaxWidth = Just height }


withMenuAttributes : List (Attribute msg) -> ViewConfig a msg -> ViewConfig a msg
withMenuAttributes attribs (ViewConfig config) =
    ViewConfig { config | menuAttributes = config.menuAttributes ++ attribs }


type OptionState
    = Idle
    | Highlighted
    | Selected


withOptionElement : (OptionState -> a -> Element msg) -> ViewConfig a msg -> ViewConfig a msg
withOptionElement toEl (ViewConfig config) =
    ViewConfig { config | optionElement = toEl }


withNoMatchElement : Element msg -> ViewConfig a msg -> ViewConfig a msg
withNoMatchElement element (ViewConfig config) =
    ViewConfig { config | noMatchElement = element }


type ClearButton msg
    = ClearButton (List (Attribute msg)) (Element msg)


withClearButton : Maybe (ClearButton msg) -> ViewConfig a msg -> ViewConfig a msg
withClearButton cb (ViewConfig config) =
    ViewConfig { config | clearButton = Maybe.map (\(ClearButton attribs label) -> clearButtonElement config.onChange attribs label) cb }


clearButton : List (Attribute msg) -> Element msg -> ClearButton msg
clearButton attribs label =
    ClearButton attribs label


toElement : Model a -> ViewConfigInternal a msg -> Element msg
toElement model config =
    Element.el
        ([ Element.htmlAttribute (Html.Attributes.id <| Model.toContainerElementId model)
         , Element.width Element.fill
         ]
            ++ (if Model.isOpen model then
                    [ Element.htmlAttribute <| Html.Attributes.style "z-index" "21" ]

                else
                    []
               )
        )
        (inputView (Model.toMenuPlacement model) (Model.toFilteredOptions config.itemToString config.filter model) model config)


inputView : Placement -> List (Option a) -> Model a -> ViewConfigInternal a msg -> Element msg
inputView placement filteredOptions model config =
    Input.text
        (config.inputAttribs
            ++ [ Events.onFocus (config.onChange InputFocused)
               , Events.onClick (config.onChange InputClicked)
               , Events.onLoseFocus (config.onChange InputLostFocus)
               , onKeyDown (KeyDown filteredOptions >> config.onChange)
               , Element.htmlAttribute (Html.Attributes.id <| Model.toInputElementId model)
               , Element.inFront <|
                    if Model.toValue model /= Nothing || Model.toInputValue model /= "" then
                        Maybe.withDefault Element.none config.clearButton

                    else
                        Element.none
               , Element.below <|
                    if List.length filteredOptions == 0 && Model.isOpen model && not (String.isEmpty (Model.toInputValue model)) && (Model.toRequestState model == Nothing || Model.toRequestState model == Just Success) then
                        config.noMatchElement

                    else
                        Element.none
               , Placement.toAttribute placement <|
                    dropdownMenu
                        (defaultDropdownAttrs
                            { menuWidth = Model.toMenuMinWidth model
                            , maxWidth = config.menuMaxWidth
                            , menuHeight = Model.toMenuMaxHeight model
                            , maxHeight = config.menuMaxHeight
                            }
                            ++ config.menuAttributes
                        )
                        { menuId = Model.toMenuElementId model
                        , toOptionId = Model.toOptionElementId model
                        , onChange = config.onChange
                        , menuOpen = Model.isOpen model
                        , options = filteredOptions
                        , highlighted = Model.toHighlighted model
                        , selected = Model.toValue model
                        , noMatchElement =
                            if Model.toInputValue model /= "" && (Model.toRequestState model /= Just NotRequested && Model.toRequestState model /= Just Loading) then
                                Element.el config.menuAttributes config.noMatchElement

                            else
                                Element.none
                        , optionElement = config.optionElement
                        }
               ]
        )
        { onChange = InputChanged >> config.onChange
        , text =
            if Model.isOpen model then
                Model.toInputValue model

            else
                Maybe.andThen (findOptionString filteredOptions) (Model.toValue model)
                    |> Maybe.withDefault (Model.toInputValue model)
        , placeholder = config.placeholder
        , label = config.label
        }


dropdownMenu :
    List (Attribute msg)
    ->
        { menuId : String
        , toOptionId : Int -> String
        , onChange : Msg a -> msg
        , menuOpen : Bool
        , options : List (Option a)
        , optionElement : OptionState -> a -> Element msg
        , highlighted : Int
        , selected : Maybe a
        , noMatchElement : Element msg
        }
    -> Element msg
dropdownMenu attribs v =
    List.indexedMap (optionElement v) v.options
        |> Element.column (attribs ++ [ Element.htmlAttribute <| Html.Attributes.id v.menuId ])
        |> Element.el
            (Element.width Element.fill
                :: (if v.menuOpen && List.length v.options > 0 then
                        []

                    else
                        [ Element.htmlAttribute (Html.Attributes.style "visibility" "hidden")
                        , Element.height (Element.px 0)
                        , Element.clipY
                        ]
                   )
            )


optionElement :
    { b
        | highlighted : Int
        , selected : Maybe a
        , onChange : Msg a -> msg
        , toOptionId : Int -> String
        , optionElement : OptionState -> a -> Element msg
    }
    -> Int
    -> Option a
    -> Element msg
optionElement v i (( a, _ ) as opt) =
    let
        optionState =
            if v.highlighted == i then
                Highlighted

            else if v.selected == Just a then
                Selected

            else
                Idle
    in
    Element.row
        [ Element.htmlAttribute (Html.Attributes.id (v.toOptionId i))
        , Element.htmlAttribute (Html.Events.preventDefaultOn "mousedown" (Decode.succeed ( v.onChange NoOp, True )))
        , Element.htmlAttribute (Html.Events.preventDefaultOn "click" (Decode.succeed ( v.onChange <| OptionClicked opt, True )))
        , Events.onMouseEnter (v.onChange <| MouseEnteredOption i)
        , Element.width Element.fill
        ]
        [ v.optionElement optionState a ]


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


defaultDropdownAttrs :
    { menuWidth : Maybe Int
    , maxWidth : Maybe Int
    , menuHeight : Maybe Int
    , maxHeight : Maybe Int
    }
    -> List (Attribute msg)
defaultDropdownAttrs { menuWidth, maxWidth, menuHeight, maxHeight } =
    [ Element.height <|
        case [ maxHeight, menuHeight ] |> List.filterMap identity of
            [ h1, h2 ] ->
                Element.maximum (Basics.min h1 h2) Element.shrink

            [ h ] ->
                Element.maximum h Element.shrink

            _ ->
                Element.shrink
    , Element.scrollbarY
    , Border.solid
    , Border.color (Element.rgb 0.8 0.8 0.8)
    , Border.width 1
    , Border.rounded 5
    , Background.color (Element.rgb 1 1 1)
    , Element.width <|
        case menuWidth of
            Just w ->
                Element.shrink
                    |> Element.minimum w
                    |> (Maybe.map Element.maximum maxWidth |> Maybe.withDefault identity)

            Nothing ->
                Element.fill
    , Element.paddingXY 0 5
    ]


defaultOptionElement : (a -> String) -> OptionState -> a -> Element msg
defaultOptionElement toString optionState a =
    let
        optionAttrs =
            [ Element.width Element.fill
            , Element.pointer
            , Element.paddingXY 14 10
            ]
    in
    case optionState of
        Highlighted ->
            Element.el
                ([ Background.color (Element.rgb 0.9 0.9 0.9)
                 , Font.color (Element.rgb 0 0 0)
                 ]
                    ++ optionAttrs
                )
                (Element.text (toString a))

        Selected ->
            Element.el
                ([ Background.color (Element.rgb 0.65 0.84 0.98)
                 , Font.color (Element.rgb 0 0 0)
                 ]
                    ++ optionAttrs
                )
                (Element.text (toString a))

        Idle ->
            Element.el optionAttrs (Element.text (toString a))


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



-- PRIVATE HELPERS


onKeyDown : (String -> msg) -> Attribute msg
onKeyDown tagger =
    Element.htmlAttribute <|
        Html.Events.preventDefaultOn "keydown"
            (Decode.map (hijackKey tagger)
                (Decode.field "key" Decode.string)
            )


hijackKey : (String -> msg) -> String -> ( msg, Bool )
hijackKey tagger key =
    ( tagger key, List.member key [ "ArrowUp", "ArrowDown", "PageUp", "PageDown" ] )


findOptionString : List (Option a) -> a -> Maybe String
findOptionString list a =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if a == Tuple.first x then
                Just (Tuple.second x)

            else
                findOptionString xs a
