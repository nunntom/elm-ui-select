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
update_ ({ request, onFocus, onLoseFocus, onInput, onKeyDown } as updateOptions) tagger msg model =
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
                |> Model.setFilteredOptions (Just filteredOptions)
                |> Model.setSelected Nothing
                |> Model.setItems
                    (if request /= Nothing && val == "" then
                        []

                     else
                        Model.toItems model
                    )
            , Maybe.map (\onInput_ -> onInput_ val) onInput
                |> Effect.emitJust
            )
                |> andThen (doDebounceRequest tagger updateOptions)

        OptionClicked closeOnSelect opt ->
            ( Model.selectOption closeOnSelect opt model
            , Effect.none
            )

        InputFocused { openMenu, isMobile } inputValue maybeOptions ->
            (case maybeOptions of
                Just ( items, options ) ->
                    Model.setItems items model
                        |> Model.setFilteredOptions (Just options)

                Nothing ->
                    model
            )
                |> Model.setInputValue inputValue
                |> Model.setIsMobile isMobile
                |> Model.setFocused True
                |> (if isMobile then
                        Model.openMenu

                    else
                        identity
                   )
                |> (if openMenu then
                        onFocusMenu tagger (request /= Nothing)

                    else
                        \m -> ( m, Effect.none )
                   )
                |> withEffect (\_ -> Effect.emitJust onFocus)

        InputClicked ->
            onFocusMenu tagger (request /= Nothing) model

        InputLostFocus config filteredOptions ->
            ( Model.blur config (request /= Nothing) filteredOptions model
            , Effect.emitJust onLoseFocus
            )

        GotNewFilteredOptions isMobile ( items, options ) ->
            ( Model.setItems items model
                |> Model.setIsMobile isMobile
                |> Model.setFilteredOptions (Just options)
            , Effect.None
            )

        MouseEnteredOption i ->
            ( Model.highlightIndex (Just i) True model
            , Effect.none
            )

        KeyDown { closeOnSelect, selectOnTab } filteredOptions key ->
            Model.setFilteredOptions (Just filteredOptions) model
                |> Model.setFocused True
                |> handleKey closeOnSelect selectOnTab tagger (request /= Nothing) key filteredOptions
                |> withEffect (\_ -> Effect.emitJust (Maybe.map (\ev -> ev key) onKeyDown))

        GotContainerAndMenuElements maybeIdx result ->
            ( model
                |> Model.setElements
                    { container = Maybe.map .container (Result.toMaybe result)
                    , menu = Maybe.map .menu (Result.toMaybe result)
                    }
                |> Model.openMenu
            , Effect.batch
                [ case maybeIdx of
                    Just idx ->
                        Effect.GetElementsAndScrollMenu
                            (tagger NoOp)
                            { menuId = Model.toMenuElementId model
                            , optionId = Model.toOptionElementId model idx
                            }

                    Nothing ->
                        Effect.none
                , Effect.FocusInput (Model.toInputElementId model) (tagger NoOp)
                ]
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

        InputDebounceReturned query ->
            if query == Model.toInputValue model then
                Maybe.map (sendRequest tagger Nothing query model) request
                    |> Maybe.withDefault ( model, Effect.none )

            else
                ( model, Effect.none )

        GotRequestResponse query response ->
            if Model.isLoadingQuery query model then
                case response of
                    Ok ( items, selected ) ->
                        ( model
                            |> Model.setItems items
                            |> Model.setSelected selected
                            |> Model.setRequestState (Just <| Success query)
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

        GotIsMobile isMobile ->
            ( Model.setIsMobile isMobile model, Effect.none )

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
    , if not hasRequest || Model.isRequestSuccess model then
        Effect.batch
            [ Effect.ScrollMenuToTop (tagger NoOp) (Model.toMenuElementId model)
            , getContainerAndMenuElementsEffect selectedIdx tagger model
            ]

      else
        Effect.none
    )


handleKey : Bool -> Bool -> (Msg a -> msg) -> Bool -> String -> List (Option a) -> Model a -> ( Model a, Effect effect msg )
handleKey closeOnSelect selectOnTab tagger hasRequest key filteredOptions model =
    let
        selectHighlighted =
            case Model.toHighlighted model |> Maybe.andThen (\idx -> getAt idx filteredOptions) of
                Just opt ->
                    ( Model.selectOption closeOnSelect opt model, Effect.none )

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


doDebounceRequest : (Msg a -> msg) -> UpdateOptions err effect a msg -> Model a -> ( Model a, Effect effect msg )
doDebounceRequest tagger ({ request, requestMinInputLength, debounceRequest } as updateOptions) model =
    case request of
        Just _ ->
            let
                shouldDebounce =
                    shouldDebounceRequest updateOptions model
            in
            ( if String.length (Model.toInputValue model) < requestMinInputLength then
                model
                    |> Model.setRequestState (Just NotRequested)
                    |> Model.setItems []

              else if shouldDebounce && not (Model.isLoading model) then
                Model.setRequestState (Just NotRequested) model

              else
                model
            , if shouldDebounce then
                Effect.Delay (InputDebounceReturned (Model.toInputValue model) |> tagger) debounceRequest

              else
                getContainerAndMenuElementsEffect Nothing tagger model
            )

        Nothing ->
            ( model
            , getContainerAndMenuElementsEffect Nothing tagger model
            )


shouldDebounceRequest : UpdateOptions err effect a msg -> Model a -> Bool
shouldDebounceRequest opts model =
    if opts.debounceRequest == 0 then
        String.length (Model.toInputValue model)
            >= opts.requestMinInputLength
            && not
                (Maybe.map (\v -> String.startsWith v (Model.toInputValue model)) (Model.toPreviousQuery model)
                    |> Maybe.withDefault False
                )

    else
        String.length (Model.toInputValue model) >= opts.requestMinInputLength


sendRequest : (Msg a -> msg) -> Maybe (a -> Bool) -> String -> Model d -> (String -> (Result b (List a) -> msg) -> effect) -> ( Model d, Effect effect msg )
sendRequest tagger selectItem query model effect =
    ( Model.setRequestState (Just <| Loading query) model
    , Effect.Request
        (effect query
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


andThen : (Model a -> ( Model a, Effect effect msg )) -> ( Model a, Effect effect msg ) -> ( Model a, Effect effect msg )
andThen f ( model, eff ) =
    let
        ( model_, eff_ ) =
            f model
    in
    ( model_, Effect.batch [ eff, eff_ ] )
