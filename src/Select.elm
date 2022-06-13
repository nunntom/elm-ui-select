module Select exposing
    ( Select
    , init, setItems, setSelected, setInputValue
    , toValue, toInputValue
    , Msg, update
    , view, toElement, withFilter, withMenuAlwaysAbove, withMenuAlwaysBelow, withMenuMaxHeight, withMenuAttributes, withNoMatchElement, OptionState(..), withOptionElement
    , Effect, updateEffect
    , clearButton, withClearButton
    )

{-| A select dropdown for Elm-Ui


# Definition

@docs Select


# Initialise and set

@docs init, setItems, setSelected, setInputValue


# Getters

@docs toValue, toInputValue


# Update

@docs Msg, update


# View

@docs view, toElement, withFilter, withMenuAlwaysAbove, withMenuAlwaysBelow, withMenuMaxHeight, withMenuAttributes, withNoMatchElement, OptionState, withOptionElement


# Effect

@docs Effect, updateEffect

-}

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onFocus, onLoseFocus, onMouseEnter)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Html.Events
import Internal
import Internal.Effect as Effect
import Internal.Filter as Filter exposing (Filter)
import Internal.List as List
import Internal.Option exposing (Option)
import Internal.Placement as Placement exposing (Placement(..))
import Json.Decode as Decode
import Select.Msg as Msg exposing (Msg(..))



-- MODEL


{-|

    The Select Type

-}
type Select a
    = Select (InternalState a)


type alias InternalState a =
    { id : String
    , items : List a
    , selected : Maybe a
    , inputValue : String
    , highlighted : Int
    , menuOpen : Bool
    , menuHeight : Maybe Int
    , menuPlacement : Placement
    }


init : String -> Select a
init id =
    Select
        { id = id
        , items = []
        , selected = Nothing
        , inputValue = ""
        , highlighted = 0
        , menuOpen = False
        , menuHeight = Nothing
        , menuPlacement = Below
        }


setItems : List a -> Select a -> Select a
setItems items (Select d) =
    Select { d | items = items }


setSelected : Maybe a -> Select a -> Select a
setSelected a (Select d) =
    Select { d | selected = a }


setInputValue : String -> Select a -> Select a
setInputValue v (Select d) =
    Select { d | inputValue = v }


toValue : Select a -> Maybe a
toValue (Select { selected }) =
    selected


toInputValue : Select a -> String
toInputValue (Select { inputValue }) =
    inputValue



-- UPDATE


type alias Msg a =
    Msg.Msg a


update : Msg.Msg a -> Select a -> ( Select a, Cmd (Msg.Msg a) )
update msg select =
    updateEffect identity msg select
        |> Tuple.mapSecond Effect.perform


updateEffect : (Msg a -> msg) -> Msg.Msg a -> Select a -> ( Select a, Effect msg )
updateEffect toMsg msg (Select state) =
    case msg of
        Msg.InputChanged val ->
            ( Select
                { state
                    | inputValue = val
                    , highlighted = 0
                    , selected = Nothing
                }
            , Effect.GetMenuHeightAndPlacement (GotMenuHeightAndPlacement >> toMsg) state.id
            )

        Msg.OptionClicked ( a, s ) ->
            ( Select
                { state
                    | selected = Just a
                    , menuOpen = False
                    , inputValue = s
                }
            , Effect.none
            )

        Msg.InputFocused ->
            ( Select
                { state | highlighted = 0 }
            , Effect.GetMenuHeightAndPlacement (GotMenuHeightAndPlacement >> toMsg) state.id
            )

        Msg.InputClicked ->
            ( Select state
            , Effect.GetMenuHeightAndPlacement (GotMenuHeightAndPlacement >> toMsg) state.id
            )

        Msg.InputLostFocus ->
            ( Select { state | menuOpen = False }
            , Effect.none
            )

        Msg.MouseEnteredOption i ->
            ( Select
                { state
                    | highlighted = i
                }
            , Effect.none
            )

        Msg.KeyDown filteredOptions key ->
            handleKey toMsg state key filteredOptions

        Msg.GotMenuHeightAndPlacement result ->
            ( Select
                { state
                    | menuOpen = True
                    , menuHeight = Maybe.andThen Tuple.first (Result.toMaybe result)
                    , menuPlacement = Maybe.map Tuple.second (Result.toMaybe result) |> Maybe.withDefault state.menuPlacement
                }
            , Effect.none
            )

        Msg.GotScrollMenuResult _ ->
            ( Select state, Effect.none )

        Msg.ClearButtonPressed ->
            ( Select { state | inputValue = "", selected = Nothing }, Effect.none )


handleKey : (Msg a -> msg) -> InternalState a -> String -> List (Option a) -> ( Select a, Effect msg )
handleKey toMsg ({ highlighted } as state) key filteredOptions =
    let
        moveHighlight newHighlighted =
            ( Select
                { state
                    | highlighted = newHighlighted
                }
            , Effect.batch
                [ Effect.GetElementsAndScrollMenu (GotScrollMenuResult >> toMsg) state.id newHighlighted
                , Effect.GetMenuHeightAndPlacement (GotMenuHeightAndPlacement >> toMsg) state.id
                ]
            )
    in
    case key of
        "ArrowDown" ->
            moveHighlight (Basics.min (List.length filteredOptions - 1) (highlighted + 1))

        "ArrowUp" ->
            moveHighlight (Basics.max 0 (highlighted - 1))

        "PageDown" ->
            moveHighlight (Basics.min (List.length filteredOptions - 1) (highlighted + 10))

        "PageUp" ->
            moveHighlight (Basics.max 0 (highlighted - 10))

        "Enter" ->
            case List.getAt highlighted filteredOptions of
                Just ( a, s ) ->
                    ( Select
                        { state
                            | menuOpen = False
                            , selected = Just a
                            , inputValue = s
                        }
                    , Effect.none
                    )

                Nothing ->
                    ( Select { state | menuOpen = False }
                    , Effect.none
                    )

        "Escape" ->
            ( Select
                { state
                    | menuOpen = False
                    , highlighted = 0
                }
            , Effect.none
            )

        _ ->
            ( Select state, Effect.none )



-- VIEW


type ViewConfig a msg
    = ViewConfig
        { onChange : Msg.Msg a -> msg
        , inputAttribs : List (Attribute msg)
        , select : Select a
        , itemToString : a -> String
        , label : Input.Label msg
        , placeholder : Maybe (Input.Placeholder msg)
        , filter : Maybe (Filter a)
        , menuPlacement : Maybe Placement
        , menuMaxHeight : Maybe Int
        , menuAttributes : List (Attribute msg)
        , noMatchElement : Element msg
        , optionElement : OptionState -> a -> Element msg
        , clearButton : Maybe (Element msg)
        }


view :
    List (Attribute msg)
    ->
        { onChange : Msg.Msg a -> msg
        , select : Select a
        , itemToString : a -> String
        , label : Input.Label msg
        , placeholder : Maybe (Input.Placeholder msg)
        }
    -> ViewConfig a msg
view attribs v =
    ViewConfig
        { onChange = v.onChange
        , inputAttribs = attribs
        , select = v.select
        , itemToString = v.itemToString
        , optionElement = defaultOptionElement v.itemToString
        , label = v.label
        , placeholder = v.placeholder
        , filter = Just Filter.startsWithThenContains
        , menuPlacement = Nothing
        , menuMaxHeight = Nothing
        , menuAttributes = defaultDropdownAttrs
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


withMenuAttributes : List (Attribute msg) -> ViewConfig a msg -> ViewConfig a msg
withMenuAttributes attribs (ViewConfig config) =
    ViewConfig { config | menuAttributes = attribs }


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


withClearButton : ClearButton msg -> ViewConfig a msg -> ViewConfig a msg
withClearButton (ClearButton attribs label) (ViewConfig config) =
    ViewConfig { config | clearButton = Just (clearButtonElement config.onChange attribs label) }


clearButton : List (Attribute msg) -> Element msg -> ClearButton msg
clearButton attribs label =
    ClearButton attribs label


toElement : ViewConfig a msg -> Element msg
toElement (ViewConfig ({ select } as config)) =
    let
        d =
            unwrap select

        filteredOptions =
            List.map (\i -> ( i, config.itemToString i )) d.items
                |> Filter.filterOptions d.inputValue config.filter

        inputVal =
            if d.menuOpen then
                d.inputValue

            else
                Maybe.andThen
                    (\sel ->
                        List.filterMap
                            (\( a, s ) ->
                                if a == sel then
                                    Just s

                                else
                                    Nothing
                            )
                            filteredOptions
                            |> List.head
                    )
                    d.selected
                    |> Maybe.withDefault d.inputValue
    in
    el
        ([ htmlAttribute (Html.Attributes.id <| d.id ++ "-element")
         , width fill
         ]
            ++ (if d.menuOpen then
                    [ htmlAttribute <| Html.Attributes.style "z-index" "21" ]

                else
                    []
               )
        )
    <|
        Input.text
            (config.inputAttribs
                ++ [ onFocus (config.onChange Msg.InputFocused)
                   , onClick (config.onChange Msg.InputClicked)
                   , onLoseFocus (config.onChange Msg.InputLostFocus)
                   , onKeyDown (Msg.KeyDown filteredOptions >> config.onChange)
                   , htmlAttribute (Html.Attributes.id <| d.id ++ "-input")
                   , inFront <| Maybe.withDefault Element.none config.clearButton
                   , Placement.toAttribute config.menuPlacement <|
                        dropdownMenu
                            { onChange = config.onChange
                            , id = d.id
                            , menuOpen = d.menuOpen
                            , options = filteredOptions
                            , highlighted = d.highlighted
                            , selected = d.selected
                            , maxHeight =
                                case ( config.menuMaxHeight, d.menuHeight ) of
                                    ( Just h1, Just h2 ) ->
                                        Just (Basics.min h1 h2)

                                    ( Just h1, Nothing ) ->
                                        Just h1

                                    ( Nothing, Just h2 ) ->
                                        Just h2

                                    _ ->
                                        Nothing
                            , menuAttributes = config.menuAttributes
                            , noMatchElement = config.noMatchElement
                            , optionElement = config.optionElement
                            }
                   ]
            )
            { onChange = Msg.InputChanged >> config.onChange
            , text = inputVal
            , placeholder = config.placeholder
            , label = config.label
            }


dropdownMenu :
    { onChange : Msg.Msg a -> msg
    , menuOpen : Bool
    , id : String
    , options : List (Option a)
    , optionElement : OptionState -> a -> Element msg
    , highlighted : Int
    , selected : Maybe a
    , maxHeight : Maybe Int
    , menuAttributes : List (Attribute msg)
    , noMatchElement : Element msg
    }
    -> Element msg
dropdownMenu v =
    if List.length v.options > 0 then
        List.indexedMap (optionElement v) v.options
            |> column
                ([ height
                    (shrink
                        |> (Maybe.map maximum v.maxHeight
                                |> Maybe.withDefault identity
                           )
                    )
                 , htmlAttribute <| Html.Attributes.id (v.id ++ "-menu")
                 , scrollbarY
                 ]
                    ++ v.menuAttributes
                )
            |> el
                (width fill
                    :: (if v.menuOpen then
                            []

                        else
                            [ htmlAttribute (Html.Attributes.style "visibility" "hidden") ]
                       )
                )

    else
        el v.menuAttributes v.noMatchElement


optionElement :
    { b
        | highlighted : Int
        , selected : Maybe a
        , onChange : Msg.Msg a -> msg
        , id : String
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
    row
        [ htmlAttribute (Html.Attributes.id <| Internal.optionId i v.id)
        , htmlAttribute (Html.Events.preventDefaultOn "mousedown" (Decode.succeed ( v.onChange <| Msg.OptionClicked opt, True )))
        , onMouseEnter (v.onChange <| Msg.MouseEnteredOption i)
        , width fill
        ]
        [ v.optionElement optionState a ]


clearButtonElement : (Msg a -> msg) -> List (Attribute msg) -> Element msg -> Element msg
clearButtonElement toMsg attribs element =
    Input.button attribs
        { onPress = Just (toMsg Msg.ClearButtonPressed)
        , label = element
        }



-- DEFAULT STYLE


defaultDropdownAttrs : List (Attribute msg)
defaultDropdownAttrs =
    [ Border.solid
    , Border.color (rgb255 180 180 180)
    , Border.width 1
    , Border.rounded 5
    , Background.color (rgb 1 1 1)
    , width fill
    , paddingXY 0 5
    ]


defaultOptionElement : (a -> String) -> OptionState -> a -> Element msg
defaultOptionElement toString optionState a =
    let
        optionAttrs =
            [ width fill
            , pointer
            , paddingXY 5 5
            ]
    in
    case optionState of
        Highlighted ->
            el
                ([ Background.color (rgb 0.5 0.5 1)
                 , Font.color (rgb 1 1 1)
                 ]
                    ++ optionAttrs
                )
                (text (toString a))

        Selected ->
            el
                ([ Background.color (rgb 0.8 0.8 1)
                 , Font.color (rgb 1 1 1)
                 ]
                    ++ optionAttrs
                )
                (text (toString a))

        Idle ->
            el optionAttrs (text (toString a))


defaultNoMatchElement : Element msg
defaultNoMatchElement =
    el [ paddingXY 5 0 ] (text "No matches")



-- PRIVATE HELPERS


unwrap : Select a -> InternalState a
unwrap (Select state) =
    state


onKeyDown : (String -> msg) -> Attribute msg
onKeyDown tagger =
    htmlAttribute <|
        Html.Events.preventDefaultOn "keydown"
            (Decode.map (hijackKey tagger)
                (Decode.field "key" Decode.string)
            )


hijackKey : (String -> msg) -> String -> ( msg, Bool )
hijackKey tagger key =
    ( tagger key, List.member key [ "ArrowUp", "ArrowDown", "PageUp", "PageDown" ] )



-- EFFECT


{-|

    For use with the Effect pattern

-}
type alias Effect msg =
    Effect.Effect msg
