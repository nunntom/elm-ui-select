module Select exposing
    ( Select
    , init, setItems, setSelected, setInputValue
    , toValue, toInputValue
    , Msg, update
    , view, toElement, withFilter, withMenuAlwaysAbove, withMenuAlwaysBelow, withMenuMaxHeight, withMenuAttributes, withNoMatchElement, OptionState(..), withOptionElement
    , Effect, updateEffect
    , RequestState, ViewConfig, clearButton, gotRequestResponse, isLoading, isOpen, isRequestFailed, request, updateEffectWithRequest, updateWithRequest, withClearButton, withMenuMaxWidth
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

import Browser.Dom as Dom
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
    | Success
    | Failed


type alias InternalState a =
    { id : String
    , items : List a
    , selected : Maybe a
    , inputValue : String
    , highlighted : Int
    , menuOpen : Bool
    , containerElement : Maybe Dom.Element
    , menuElement : Maybe Dom.Element
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
        , containerElement = Nothing
        , menuElement = Nothing
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


isOpen : Select a -> Bool
isOpen (Select { menuOpen }) =
    menuOpen


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
            , case maybeRequest of
                Just req ->
                    if String.length val >= Request.toMinLength req then
                        Effect.Debounce (Request.toDelay req) (InputDebounceReturned val |> toMsg)

                    else
                        Effect.none

                Nothing ->
                    Effect.GetContainerAndMenuElements (GotContainerAndMenuElements >> toMsg) state.id
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
            , Effect.GetContainerAndMenuElements (GotContainerAndMenuElements >> toMsg) state.id
            )

        InputClicked ->
            ( Select state
            , Effect.GetContainerAndMenuElements (GotContainerAndMenuElements >> toMsg) state.id
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

        GotContainerAndMenuElements result ->
            ( Select
                { state
                    | menuOpen = True
                    , menuElement = Maybe.map .menu (Result.toMaybe result)
                    , containerElement = Maybe.map .container (Result.toMaybe result)
                }
            , Effect.none
            )

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
                    , requestState = Just Success
                }
            , Effect.GetContainerAndMenuElements (GotContainerAndMenuElements >> toMsg) state.id
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
                , Effect.GetContainerAndMenuElements (GotContainerAndMenuElements >> toMsg) state.id
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
    -> ViewConfig a msg
view attribs v =
    ViewConfig
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


withClearButton : ClearButton msg -> ViewConfig a msg -> ViewConfig a msg
withClearButton (ClearButton attribs label) (ViewConfig config) =
    ViewConfig { config | clearButton = Just (clearButtonElement config.onChange attribs label) }


clearButton : List (Attribute msg) -> Element msg -> ClearButton msg
clearButton attribs label =
    ClearButton attribs label


toElement : Select a -> ViewConfig a msg -> Element msg
toElement ((Select s) as select) (ViewConfig config) =
    let
        dimensionsAndPlacement =
            Maybe.map2 calculateMenuDimensionsAndPlacement s.containerElement s.menuElement

        placement =
            case config.menuPlacement of
                Just p ->
                    p

                Nothing ->
                    Maybe.map .placement dimensionsAndPlacement
                        |> Maybe.withDefault Placement.Below

        filteredOptions =
            List.map (\i -> ( i, config.itemToString i )) s.items
                |> Filter.filterOptions s.inputValue config.filter
    in
    el
        ([ htmlAttribute (Html.Attributes.id <| s.id ++ "-element")
         , width fill
         ]
            ++ (if s.menuOpen then
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
                   , htmlAttribute (Html.Attributes.id <| s.id ++ "-input")
                   , inFront <|
                        if toValue select /= Nothing || toInputValue select /= "" then
                            Maybe.withDefault Element.none config.clearButton

                        else
                            Element.none
                   , Element.below <|
                        if List.length filteredOptions == 0 && s.menuOpen && not (String.isEmpty s.inputValue) && (s.requestState == Nothing || s.requestState == Just Success) then
                            config.noMatchElement

                        else
                            Element.none
                   , Placement.toAttribute placement <|
                        dropdownMenu
                            (defaultDropdownAttrs
                                { menuWidth = Maybe.map .minWidth dimensionsAndPlacement
                                , maxWidth = config.menuMaxWidth
                                , menuHeight = Maybe.map .maxHeight dimensionsAndPlacement
                                , maxHeight = config.menuMaxHeight
                                }
                                ++ config.menuAttributes
                            )
                            { id = s.id
                            , onChange = config.onChange
                            , menuOpen = s.menuOpen
                            , options = filteredOptions
                            , highlighted = s.highlighted
                            , selected = s.selected
                            , noMatchElement =
                                if s.inputValue /= "" && (s.requestState /= Just NotRequested && s.requestState /= Just Loading) then
                                    el config.menuAttributes config.noMatchElement

                                else
                                    Element.none
                            , optionElement = config.optionElement
                            }
                   ]
            )
            { onChange = InputChanged >> config.onChange
            , text =
                if s.menuOpen then
                    s.inputValue

                else
                    Maybe.andThen (findOptionString filteredOptions) s.selected
                        |> Maybe.withDefault s.inputValue
            , placeholder = config.placeholder
            , label = config.label
            }


dropdownMenu :
    List (Attribute msg)
    ->
        { id : String
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
        |> column (attribs ++ [ htmlAttribute <| Html.Attributes.id (v.id ++ "-menu") ])
        |> el
            (width fill
                :: (if v.menuOpen && List.length v.options > 0 then
                        []

                    else
                        [ htmlAttribute (Html.Attributes.style "visibility" "hidden")
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


defaultDropdownAttrs :
    { menuWidth : Maybe Int
    , maxWidth : Maybe Int
    , menuHeight : Maybe Int
    , maxHeight : Maybe Int
    }
    -> List (Attribute msg)
defaultDropdownAttrs { menuWidth, maxWidth, menuHeight, maxHeight } =
    [ height <|
        case [ maxHeight, menuHeight ] |> List.filterMap identity of
            [ h1, h2 ] ->
                maximum (Basics.min h1 h2) shrink

            [ h ] ->
                maximum h shrink

            _ ->
                shrink
    , scrollbarY
    , Border.solid
    , Border.color (rgb255 180 180 180)
    , Border.width 1
    , Border.rounded 5
    , Background.color (rgb 1 1 1)
    , width <|
        case menuWidth of
            Just w ->
                shrink
                    |> minimum w
                    |> (Maybe.map maximum maxWidth |> Maybe.withDefault identity)

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


calculateMenuDimensionsAndPlacement : Dom.Element -> Dom.Element -> { minWidth : Int, maxHeight : Int, placement : Placement }
calculateMenuDimensionsAndPlacement container menu =
    let
        { above, below } =
            calculateSpace container
    in
    if below < Basics.round menu.scene.height && above > below then
        { minWidth = Basics.round container.element.width
        , maxHeight = Basics.min (Basics.round menu.scene.height) (above - 10)
        , placement = Above
        }

    else
        { minWidth = Basics.round container.element.width
        , maxHeight = Basics.min (Basics.round menu.scene.height) (below - 10)
        , placement = Below
        }


calculateSpace : Dom.Element -> { above : Int, below : Int }
calculateSpace { viewport, element } =
    { above = Basics.round (element.y - viewport.y)
    , below =
        Basics.round
            ((viewport.y + viewport.height)
                - (element.y + element.height)
            )
    }


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



-- EFFECT


{-|

    For use with the Effect pattern

-}
type alias Effect eff msg =
    Effect.Effect eff msg
