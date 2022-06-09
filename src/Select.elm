module Select exposing
    ( Msg
    , OptionState(..)
    , Select
    , close
    , init
    , isOpen
    , open
    , setItems
    , setSelected
    , toElement
    , toInputValue
    , toValue
    , update
    , view
    , withFilter
    , withMenuAlwaysAbove
    , withMenuAlwaysBelow
    , withMenuAttributes
    , withMenuMaxHeight
    , withNoMatchElement
    , withOptionElement
    )

import Browser.Dom as Dom
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onFocus, onLoseFocus, onMouseEnter)
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events
import Internal.Filter as Filter exposing (Filter)
import Internal.Placement as Placement exposing (Placement(..))
import Json.Decode as Decode
import List.Extra as List
import Task exposing (Task)



-- MODEL


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


toValue : Select a -> Maybe a
toValue (Select { selected }) =
    selected


toInputValue : Select a -> String
toInputValue (Select { inputValue }) =
    inputValue



-- VIEW


type OptionState
    = Idle
    | Highlighted
    | Selected


type alias Option a =
    ( a, String )


type alias ViewConfigInternal a msg =
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
    -> ViewConfigInternal a msg
view attribs v =
    let
        d =
            unwrap v.select
    in
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
    }


withFilter : Filter a -> ViewConfigInternal a msg -> ViewConfigInternal a msg
withFilter filter config =
    { config | filter = Just filter }


withMenuAlwaysAbove : ViewConfigInternal a msg -> ViewConfigInternal a msg
withMenuAlwaysAbove config =
    { config | menuPlacement = Just Above }


withMenuAlwaysBelow : ViewConfigInternal a msg -> ViewConfigInternal a msg
withMenuAlwaysBelow config =
    { config | menuPlacement = Just Below }


withMenuMaxHeight : Int -> ViewConfigInternal a msg -> ViewConfigInternal a msg
withMenuMaxHeight height config =
    { config | menuMaxHeight = Just height }


withMenuAttributes : List (Attribute msg) -> ViewConfigInternal a msg -> ViewConfigInternal a msg
withMenuAttributes attribs config =
    { config | menuAttributes = attribs }


withOptionElement : (OptionState -> a -> Element msg) -> ViewConfigInternal a msg -> ViewConfigInternal a msg
withOptionElement toEl config =
    { config | optionElement = toEl }


withNoMatchElement : Element msg -> ViewConfigInternal a msg -> ViewConfigInternal a msg
withNoMatchElement element config =
    { config | noMatchElement = element }


toElement : ViewConfigInternal a msg -> Element msg
toElement ({ select } as config) =
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
                            [ htmlAttribute (Html.Attributes.style "visibility" "hidden") ]
                       )
                )

    else
        el v.menuAttributes v.noMatchElement


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
        [ htmlAttribute (Html.Attributes.id <| optionId i v.id)
        , htmlAttribute (Html.Events.preventDefaultOn "mousedown" (Decode.succeed ( v.onChange <| OptionClicked opt, True )))
        , onMouseEnter (v.onChange <| MouseEnteredOption i)
        , width fill
        ]
        [ v.optionElement optionState a ]



-- UPDATE


type Msg a
    = InputChanged String
    | OptionClicked (Option a)
    | InputFocused
    | InputClicked
    | InputLostFocus
    | MouseEnteredOption Int
    | KeyDown (List (Option a)) String
    | GotMenuHeightAndPlacement (Result Dom.Error ( Maybe Int, Placement ))
    | GotScrollMenuResult (Result Dom.Error ())


update : Msg a -> Select a -> ( Select a, Cmd (Msg a) )
update msg (Select state) =
    case msg of
        InputChanged val ->
            ( Select
                { state
                    | inputValue = val
                    , highlighted = 0
                    , selected = Nothing
                }
            , getMenuHeightAndPlacement state.id
            )

        OptionClicked ( a, s ) ->
            ( Select
                { state
                    | selected = Just a
                    , menuOpen = False
                    , inputValue = s
                }
            , Cmd.none
            )

        InputFocused ->
            ( Select
                { state | highlighted = 0 }
            , getMenuHeightAndPlacement state.id
            )

        InputClicked ->
            ( Select state
            , getMenuHeightAndPlacement state.id
            )

        InputLostFocus ->
            ( Select { state | menuOpen = False }
            , Cmd.none
            )

        MouseEnteredOption i ->
            ( Select
                { state
                    | highlighted = i
                }
            , Cmd.none
            )

        KeyDown filteredOptions key ->
            handleKey state key filteredOptions

        GotMenuHeightAndPlacement result ->
            ( Select
                { state
                    | menuOpen = True
                    , menuHeight = Maybe.andThen Tuple.first (Result.toMaybe result)
                    , menuPlacement = Maybe.map Tuple.second (Result.toMaybe result) |> Maybe.withDefault state.menuPlacement
                }
            , Cmd.none
            )

        GotScrollMenuResult _ ->
            ( Select state, Cmd.none )


handleKey : InternalState a -> String -> List (Option a) -> ( Select a, Cmd (Msg a) )
handleKey ({ highlighted } as state) key filteredOptions =
    let
        moveHighlight newHighlighted =
            ( Select
                { state
                    | highlighted = newHighlighted
                }
            , Cmd.batch
                [ getElementsAndScrollMenu state.id newHighlighted
                , getMenuHeightAndPlacement state.id
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
                    , Cmd.none
                    )

                Nothing ->
                    ( Select { state | menuOpen = False }
                    , Cmd.none
                    )

        "Escape" ->
            ( Select
                { state
                    | menuOpen = False
                    , highlighted = 0
                }
            , Cmd.none
            )

        _ ->
            ( Select state, Cmd.none )


getMenuHeightAndPlacement : String -> Cmd (Msg a)
getMenuHeightAndPlacement id =
    Task.map2
        (\input menu ->
            let
                { above, below } =
                    calculateSpace input
            in
            if below < Basics.round menu.scene.height && above > below then
                ( Just <| Basics.min (Basics.round menu.scene.height) (above - 10)
                , Above
                )

            else
                ( Just <| Basics.min (Basics.round menu.scene.height) (below - 10)
                , Below
                )
        )
        (Dom.getElement (id ++ "-input"))
        (Dom.getElement (id ++ "-menu"))
        |> Task.attempt GotMenuHeightAndPlacement


getElementsAndScrollMenu : String -> Int -> Cmd (Msg a)
getElementsAndScrollMenu id highlightedOption =
    Task.map3
        (\option menu menuViewport ->
            { option = option
            , menu = menu
            , menuViewport = menuViewport
            }
        )
        (Dom.getElement (optionId highlightedOption id))
        (Dom.getElement (id ++ "-menu"))
        (Dom.getViewportOf (id ++ "-menu"))
        |> Task.andThen (scrollMenuTask id)
        |> Task.attempt GotScrollMenuResult


scrollMenuTask : String -> { option : Dom.Element, menu : Dom.Element, menuViewport : Dom.Viewport } -> Task Dom.Error ()
scrollMenuTask id { option, menu, menuViewport } =
    let
        optionTop =
            option.element.y - menu.element.y + menuViewport.viewport.y

        optionBottom =
            optionTop + option.element.height

        scrollTop =
            if optionBottom > (menu.element.height + menuViewport.viewport.y) then
                optionBottom - menu.element.height

            else if optionTop < menuViewport.viewport.y then
                optionTop

            else
                menuViewport.viewport.y
    in
    Dom.setViewportOf (id ++ "-menu") 0 scrollTop



-- PUBLIC HELPERS


open : Select a -> Select a
open (Select state) =
    Select
        { state
            | menuOpen = True
        }


close : Select a -> Select a
close (Select state) =
    Select { state | menuOpen = False }


isOpen : Select a -> Bool
isOpen (Select { menuOpen }) =
    menuOpen



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


optionId : Int -> String -> String
optionId i id =
    id ++ "-" ++ String.fromInt i


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


calculateSpace : Dom.Element -> { above : Int, below : Int }
calculateSpace { viewport, element } =
    { above = Basics.round (element.y - viewport.y)
    , below =
        Basics.round
            ((viewport.y + viewport.height)
                - (element.y + element.height)
            )
    }
