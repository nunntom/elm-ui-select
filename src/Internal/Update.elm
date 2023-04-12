module Internal.Update exposing (sendRequest, update)

import Internal.Effect as Effect exposing (Effect)
import Internal.List.Extra as List
import Internal.Model as Model exposing (Model)
import Internal.Msg exposing (Msg(..))
import Internal.Option as Option exposing (Option)
import Internal.RequestState exposing (RequestState(..))
import Internal.UpdateOptions exposing (UpdateOptions)


update : UpdateOptions err effect a msg -> (Msg a -> msg) -> Msg a -> Model a -> ( Model a, Effect effect msg )
update ({ onSelect } as options) tagger msg model =
    update_ options tagger msg model
        |> withEffect
            (\model_ ->
                if Model.toValue model /= Model.toValue model_ then
                    Maybe.map (\onSelect_ -> onSelect_ (Model.toValue model_)) onSelect
                        |> Effect.emitJust

                else
                    Effect.none
            )


update_ : UpdateOptions err effect a msg -> (Msg a -> msg) -> Msg a -> Model a -> ( Model a, Effect effect msg )
update_ { request, requestMinInputLength, debounceRequest, onFocus, onLoseFocus, onInput } tagger msg model =
    case msg of
        InputChanged val filteredOptions ->
            ( model
                |> Model.onInputChange val
                |> Model.highlightIndex
                    (if String.isEmpty val then
                        Nothing

                     else
                        Just 0
                    )
                    False
                |> Model.setFilteredOptions filteredOptions
                |> Model.setSelected Nothing
                |> Model.setItems
                    (if request /= Nothing && val == "" then
                        []

                     else
                        Model.toItems model
                    )
                |> Model.setRequestState
                    (if request /= Nothing then
                        Just NotRequested

                     else
                        Nothing
                    )
            , Effect.batch
                [ case request of
                    Just _ ->
                        if String.length val >= requestMinInputLength then
                            Effect.Debounce (InputDebounceReturned >> tagger) debounceRequest val

                        else
                            Effect.none

                    Nothing ->
                        getContainerAndMenuElementsEffect Nothing tagger model
                , Maybe.map (\onInput_ -> onInput_ val) onInput
                    |> Effect.emitJust
                ]
            )

        OptionClicked opt ->
            ( Model.selectOption opt model
            , Effect.none
            )

        InputFocused openMenu inputValue maybeOptions ->
            (case maybeOptions of
                Just ( items, options ) ->
                    Model.setItems items model
                        |> Model.setFilteredOptions options

                Nothing ->
                    model
            )
                |> Model.setInputValue inputValue
                |> (if openMenu then
                        onFocusMenu tagger (request /= Nothing)

                    else
                        \m -> ( Model.setFocused True m, Effect.none )
                   )
                |> withEffect (\_ -> Effect.emitJust onFocus)

        InputClicked ->
            onFocusMenu tagger (request /= Nothing) model

        InputLostFocus config filteredOptions ->
            ( Model.blur config (request /= Nothing) filteredOptions model
            , Effect.emitJust onLoseFocus
            )

        GotNewFilteredOptions ( items, options ) ->
            ( Model.setItems items model
                |> Model.setFilteredOptions options
            , Effect.None
            )

        MouseEnteredOption i ->
            ( Model.highlightIndex (Just i) True model
            , Effect.none
            )

        KeyDown selectOnTab filteredOptions key ->
            Model.setFilteredOptions filteredOptions model
                |> handleKey selectOnTab tagger (request /= Nothing) key filteredOptions

        GotContainerAndMenuElements maybeIdx result ->
            ( model
                |> Model.setElements
                    { container = Maybe.map .container (Result.toMaybe result)
                    , menu = Maybe.map .menu (Result.toMaybe result)
                    }
                |> Model.openMenu
            , case maybeIdx of
                Just idx ->
                    Effect.GetElementsAndScrollMenu
                        (tagger NoOp)
                        { menuId = Model.toMenuElementId model
                        , optionId = Model.toOptionElementId model idx
                        }

                Nothing ->
                    Effect.none
            )

        ClearButtonPressed ->
            ( model
                |> Model.clear
                |> Model.setItems
                    (if request == Nothing then
                        Model.toItems model

                     else
                        []
                    )
            , Effect.FocusInput (Model.toInputElementId model) (tagger NoOp)
            )

        InputDebounceReturned val ->
            if val == Model.toInputValue model then
                Maybe.map (sendRequest tagger Nothing model) request
                    |> Maybe.withDefault ( model, Effect.none )

            else
                ( model, Effect.none )

        GotRequestResponse inputVal response ->
            if inputVal == Model.toInputValue model then
                case response of
                    Ok ( items, selected ) ->
                        ( model
                            |> Model.setItems items
                            |> Model.setSelected selected
                            |> Model.setRequestState (Just Success)
                        , if Model.toValue model == Nothing then
                            getContainerAndMenuElementsEffect Nothing tagger model

                          else
                            Effect.none
                        )

                    Err _ ->
                        ( Model.setRequestState (Just Failed) model
                        , Effect.none
                        )

            else
                ( model, Effect.none )

        NoOp ->
            ( model, Effect.none )


onFocusMenu : (Msg a -> msg) -> Bool -> Model a -> ( Model a, Effect effect msg )
onFocusMenu tagger hasRequest model =
    let
        selectedIdx =
            Model.toValue model
                |> Maybe.andThen (Option.findIndex (Model.toCurrentFilteredOptions model))
    in
    ( Model.setFocused True model
        |> Model.highlightIndex selectedIdx False
    , if not hasRequest || Model.toRequestState model == Just Success then
        Effect.batch
            [ Effect.ScrollMenuToTop (tagger NoOp) (Model.toMenuElementId model)
            , getContainerAndMenuElementsEffect selectedIdx tagger model
            ]

      else
        Effect.none
    )


handleKey : Bool -> (Msg a -> msg) -> Bool -> String -> List (Option a) -> Model a -> ( Model a, Effect effect msg )
handleKey selectOnTab tagger hasRequest key filteredOptions model =
    let
        selectHighlighted =
            case Model.toHighlighted model |> Maybe.andThen (\idx -> getAt idx filteredOptions) of
                Just opt ->
                    ( Model.selectOption opt model, Effect.none )

                Nothing ->
                    ( Model.closeMenu model, Effect.none )
    in
    case key of
        "ArrowDown" ->
            moveHighlight tagger hasRequest (Basics.min (List.length filteredOptions - 1) (Maybe.withDefault -1 (Model.toHighlighted model) + 1)) model

        "ArrowUp" ->
            moveHighlight tagger hasRequest (Basics.max 0 (Maybe.withDefault 0 (Model.toHighlighted model) - 1)) model

        "PageDown" ->
            moveHighlight tagger hasRequest (Basics.min (List.length filteredOptions - 1) (Maybe.withDefault -1 (Model.toHighlighted model) + 10)) model

        "PageUp" ->
            moveHighlight tagger hasRequest (Basics.max 0 (Maybe.withDefault 0 (Model.toHighlighted model) - 10)) model

        "Enter" ->
            selectHighlighted

        "Escape" ->
            ( Model.closeMenu model, Effect.none )

        "Tab" ->
            if selectOnTab && not (Model.wasHighlightedByMouse model) then
                selectHighlighted

            else
                ( model, Effect.none )

        _ ->
            ( model, Effect.none )


moveHighlight : (Msg a -> msg) -> Bool -> Int -> Model a -> ( Model a, Effect effect msg )
moveHighlight tagger hasRequest newHighlighted model =
    if Model.isOpen model then
        ( Model.highlightIndex (Just newHighlighted) False model
        , getContainerAndMenuElementsEffect (Just newHighlighted) tagger model
        )

    else
        onFocusMenu tagger hasRequest model


getContainerAndMenuElementsEffect : Maybe Int -> (Msg a -> msg) -> Model a -> Effect effect msg
getContainerAndMenuElementsEffect maybeIdx tagger model =
    Effect.GetContainerAndMenuElements
        (GotContainerAndMenuElements maybeIdx >> tagger)
        { menuId = Model.toMenuElementId model
        , containerId = Model.toRelativeContainerMarkerId model
        }


sendRequest : (Msg a -> msg) -> Maybe (a -> Bool) -> Model d -> (String -> (Result b (List a) -> msg) -> effect) -> ( Model d, Effect effect msg )
sendRequest tagger selectItem model effect =
    ( Model.setRequestState (Just Loading) model
    , Effect.Request
        (effect (Model.toInputValue model)
            (\res ->
                Result.mapError (\_ -> "") res
                    |> Result.map
                        (\items ->
                            case selectItem of
                                Just selectItem_ ->
                                    ( items, List.find selectItem_ items )

                                Nothing ->
                                    ( items, Nothing )
                        )
                    |> GotRequestResponse (Model.toInputValue model)
                    |> tagger
            )
        )
    )


getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs


withEffect : (Model a -> Effect effect msg) -> ( Model a, Effect effect msg ) -> ( Model a, Effect effect msg )
withEffect toEffect ( model, eff ) =
    ( model, Effect.batch [ eff, toEffect model ] )
