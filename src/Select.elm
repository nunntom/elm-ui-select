module Select exposing
    ( Select
    , init, setItems, setSelected, setInputValue
    , toValue, toInputValue
    , Msg, update
    , view, toElement, withFilter, withMenuAlwaysAbove, withMenuAlwaysBelow, withMenuMaxHeight, withMenuAttributes, withNoMatchElement, OptionState(..), withOptionElement
    , Effect, updateEffect
    , RequestState, clearButton, gotRequestResponse, isLoading, isRequestFailed, request, updateEffectWithRequest, updateWithRequest, withClearButton
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
import Internal.Msg as Msg exposing (Msg(..))
import Internal.Option exposing (Option)
import Internal.Placement as Placement exposing (Placement(..))
import Json.Decode as Decode
import Select.Request as Request exposing (Request)



-- MODEL


{-|

    The Select Type

-}
type Select a
    = Select (InternalState a)


type RequestState
    = NotRequested
    | Loading
    | Failed


type alias InternalState a =
    { id : String
    , items : List a
    , selected : Maybe a
    , inputValue : String
    , highlighted : Int
    , menuOpen : Bool
    , menuHeight : Maybe Int
    , menuPlacement : Placement
    , menuWidth : Maybe Int
    , requestState : Maybe RequestState
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
        , menuWidth = Nothing
        , requestState = Nothing
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



-- CHECKS


isLoading : Select a -> Bool
isLoading (Select { requestState }) =
    requestState == Just Loading


isRequestFailed : Select a -> Bool
isRequestFailed (Select { requestState }) =
    requestState == Just Failed



-- UPDATE


type alias Msg a =
    Msg.Msg a


update : Msg a -> Select a -> ( Select a, Cmd (Msg a) )
update msg select =
    updateEffect identity msg select
        |> Tuple.mapSecond (Effect.perform (\_ -> Cmd.none))


type alias Request eff =
    Request.Request eff


request : (String -> eff) -> Request eff
request =
    Request.request


updateWithRequest : Request (Cmd (Msg a)) -> Msg a -> Select a -> ( Select a, Cmd (Msg a) )
updateWithRequest requestCmd msg select =
    updateEffectInternal (Just (Request.request identity)) identity msg select
        |> Tuple.mapSecond (Effect.perform (Request.toEffect requestCmd))


updateEffect : (Msg a -> msg) -> Msg a -> Select a -> ( Select a, Effect Never msg )
updateEffect =
    updateEffectInternal Nothing


updateEffectWithRequest : Request eff -> (Msg a -> msg) -> Msg a -> Select a -> ( Select a, Effect eff msg )
updateEffectWithRequest req =
    updateEffectInternal (Just req)


updateEffectInternal : Maybe (Request eff) -> (Msg a -> msg) -> Msg a -> Select a -> ( Select a, Effect eff msg )
updateEffectInternal maybeRequest toMsg msg (Select state) =
    case msg of
        InputChanged val ->
            ( Select
                { state
                    | inputValue = val
                    , highlighted = 0
                    , selected = Nothing
                    , items =
                        if maybeRequest /= Nothing && val == "" then
                            []

                        else
                            state.items
                    , requestState =
                        if maybeRequest /= Nothing then
                            Just NotRequested

                        else
                            state.requestState
                }
            , Effect.batch
                [ Effect.GetMenuDimensionsAndPlacement (GotMenuDimensionsAndPlacement >> toMsg) state.id
                , case maybeRequest of
                    Just req ->
                        if String.length val >= Request.toMinLength req then
                            Effect.Debounce (Request.toDelay req) (InputDebounceReturned val |> toMsg)

                        else
                            Effect.none

                    Nothing ->
                        Effect.none
                ]
            )

        OptionClicked ( a, s ) ->
            ( Select
                { state
                    | selected = Just a
                    , menuOpen = False
                    , inputValue = s
                }
            , Effect.none
            )

        InputFocused ->
            ( Select
                { state | highlighted = 0 }
            , Effect.GetMenuDimensionsAndPlacement (GotMenuDimensionsAndPlacement >> toMsg) state.id
            )

        InputClicked ->
            ( Select state
            , Effect.GetMenuDimensionsAndPlacement (GotMenuDimensionsAndPlacement >> toMsg) state.id
            )

        InputLostFocus ->
            ( Select { state | menuOpen = False }
            , Effect.none
            )

        MouseEnteredOption i ->
            ( Select
                { state
                    | highlighted = i
                }
            , Effect.none
            )

        KeyDown filteredOptions key ->
            handleKey toMsg state key filteredOptions

        GotMenuDimensionsAndPlacement (Ok ( { width, height }, placement )) ->
            ( Select
                { state
                    | menuOpen = True
                    , menuHeight = Just height
                    , menuWidth = Just width
                    , menuPlacement = placement
                }
            , Effect.none
            )

        GotMenuDimensionsAndPlacement (Err _) ->
            ( Select state, Effect.none )

        GotScrollMenuResult _ ->
            ( Select state, Effect.none )

        ClearButtonPressed ->
            ( Select
                { state
                    | inputValue = ""
                    , selected = Nothing
                    , items =
                        if maybeRequest == Nothing then
                            state.items

                        else
                            []
                }
            , Effect.none
            )

        InputDebounceReturned val ->
            if val == state.inputValue then
                ( Select { state | requestState = Just Loading }
                , Maybe.map (Request.toEffect >> (\eff -> Effect.Request (eff val))) maybeRequest
                    |> Maybe.withDefault Effect.none
                )

            else
                ( Select state, Effect.none )

        GotRequestResponse (Ok items) ->
            ( Select
                { state
                    | items = items
                    , requestState = Nothing
                }
            , Effect.GetMenuDimensionsAndPlacement (GotMenuDimensionsAndPlacement >> toMsg) state.id
            )

        GotRequestResponse (Err _) ->
            ( Select { state | requestState = Just Failed }
            , Effect.none
            )


handleKey : (Msg a -> msg) -> InternalState a -> String -> List (Option a) -> ( Select a, Effect eff msg )
handleKey toMsg ({ highlighted } as state) key filteredOptions =
    let
        moveHighlight newHighlighted =
            ( Select
                { state
                    | highlighted = newHighlighted
                }
            , Effect.batch
                [ Effect.GetElementsAndScrollMenu (GotScrollMenuResult >> toMsg) state.id newHighlighted
                , Effect.GetMenuDimensionsAndPlacement (GotMenuDimensionsAndPlacement >> toMsg) state.id
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


gotRequestResponse : Result err (List a) -> Msg a
gotRequestResponse =
    Result.mapError (\_ -> ()) >> GotRequestResponse



-- VIEW


type ViewConfig a msg
    = ViewConfig
        { onChange : Msg a -> msg
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
        { onChange : Msg a -> msg
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
        , menuAttributes = defaultDropdownAttrs (unwrap v.select).menuWidth
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
                ++ [ onFocus (config.onChange InputFocused)
                   , onClick (config.onChange InputClicked)
                   , onLoseFocus (config.onChange InputLostFocus)
                   , onKeyDown (KeyDown filteredOptions >> config.onChange)
                   , htmlAttribute (Html.Attributes.id <| d.id ++ "-input")
                   , inFront <|
                        if toValue select /= Nothing || toInputValue select /= "" then
                            Maybe.withDefault Element.none config.clearButton

                        else
                            Element.none
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
                            , noMatchElement =
                                if d.inputValue /= "" && (d.requestState /= Just NotRequested && d.requestState /= Just Loading) then
                                    el config.menuAttributes config.noMatchElement

                                else
                                    Element.none
                            , optionElement = config.optionElement
                            }
                   ]
            )
            { onChange = InputChanged >> config.onChange
            , text = inputVal
            , placeholder = config.placeholder
            , label = config.label
            }


dropdownMenu :
    { onChange : Msg a -> msg
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
                            [ htmlAttribute (Html.Attributes.style "visibility" "hidden")
                            , Element.height (Element.px 0)
                            , Element.clipY
                            ]
                       )
                )

    else if v.menuOpen then
        v.noMatchElement

    else
        Element.none


optionElement :
    { b
        | highlighted : Int
        , selected : Maybe a
        , onChange : Msg a -> msg
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
        , htmlAttribute (Html.Events.preventDefaultOn "mousedown" (Decode.succeed ( v.onChange <| OptionClicked opt, True )))
        , onMouseEnter (v.onChange <| MouseEnteredOption i)
        , width fill
        ]
        [ v.optionElement optionState a ]


clearButtonElement : (Msg a -> msg) -> List (Attribute msg) -> Element msg -> Element msg
clearButtonElement toMsg attribs element =
    Input.button attribs
        { onPress = Just (toMsg ClearButtonPressed)
        , label = element
        }



-- DEFAULT STYLE


defaultDropdownAttrs : Maybe Int -> List (Attribute msg)
defaultDropdownAttrs menuWidth =
    [ Border.solid
    , Border.color (rgb255 180 180 180)
    , Border.width 1
    , Border.rounded 5
    , Background.color (rgb 1 1 1)
    , width <|
        case menuWidth of
            Just w ->
                minimum (Maybe.withDefault 0 menuWidth) shrink

            Nothing ->
                fill
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
type alias Effect eff msg =
    Effect.Effect eff msg
