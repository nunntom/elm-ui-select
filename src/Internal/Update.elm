module Internal.Update exposing (update)

import Internal.Effect as Effect exposing (Effect)
import Internal.Model as Model exposing (Model)
import Internal.Msg exposing (Msg(..))
import Internal.Option exposing (Option)
import Internal.RequestState exposing (RequestState(..))
import Internal.UpdateOptions exposing (UpdateOptions)


update : UpdateOptions effect a msg -> (Msg a -> msg) -> Msg a -> Model a -> ( Model a, Effect effect msg )
update ({ onSelect } as options) tagger msg model =
    let
        ( model_, effect ) =
            update_ options tagger msg model
    in
    ( model_
    , Effect.batch
        [ effect
        , case ( Model.toValue model /= Model.toValue model_, onSelect ) of
            ( True, Just onSelect_ ) ->
                Effect.Emit (onSelect_ <| Model.toValue model_)

            _ ->
                Effect.none
        ]
    )


update_ : UpdateOptions effect a msg -> (Msg a -> msg) -> Msg a -> Model a -> ( Model a, Effect effect msg )
update_ { request, requestMinInputLength, debounceRequest } tagger msg model =
    case msg of
        InputChanged val ->
            ( model
                |> Model.setInputValue val
                |> Model.highlightIndex 0
                |> Model.applyFilter True
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
            , case request of
                Just _ ->
                    if String.length val >= requestMinInputLength then
                        Effect.Debounce (InputDebounceReturned >> tagger) debounceRequest val

                    else
                        Effect.none

                Nothing ->
                    getContainerAndMenuElementsEffect tagger model
            )

        OptionClicked opt ->
            ( Model.selectOption opt model
            , Effect.none
            )

        InputFocused ->
            onFocusMenu tagger (request /= Nothing) model

        InputClicked ->
            onFocusMenu tagger (request /= Nothing) model

        InputLostFocus config filteredOptions ->
            ( Model.blur config (request /= Nothing) filteredOptions model
            , Effect.none
            )

        MouseEnteredOption i ->
            ( Model.highlightIndex i model
            , Effect.none
            )

        KeyDown filteredOptions key ->
            handleKey tagger model key filteredOptions

        GotContainerAndMenuElements result ->
            ( model
                |> Model.setElements
                    { container = Maybe.map .container (Result.toMaybe result)
                    , menu = Maybe.map .menu (Result.toMaybe result)
                    }
                |> Model.openMenu
            , Effect.none
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
            , Effect.none
            )

        InputDebounceReturned val ->
            if val == Model.toInputValue model then
                ( Model.setRequestState (Just Loading) model
                , Maybe.map (\effect -> Effect.Request (effect val)) request
                    |> Maybe.withDefault Effect.none
                )

            else
                ( model, Effect.none )

        GotRequestResponse inputVal response ->
            if inputVal == Model.toInputValue model then
                case response of
                    Ok items ->
                        ( model
                            |> Model.setItems items
                            |> Model.setRequestState (Just Success)
                        , if Model.toValue model == Nothing then
                            getContainerAndMenuElementsEffect tagger model

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
    ( Model.setFocused True model
        |> Model.highlightIndex 0
    , if not hasRequest || Model.toRequestState model == Just Success then
        Effect.batch
            [ Effect.ScrollMenuToTop (tagger NoOp) (Model.toMenuElementId model)
            , getContainerAndMenuElementsEffect tagger model
            ]

      else
        Effect.none
    )


handleKey : (Msg a -> msg) -> Model a -> String -> List (Option a) -> ( Model a, Effect effect msg )
handleKey tagger model key filteredOptions =
    case key of
        "ArrowDown" ->
            moveHighlight tagger (Basics.min (List.length filteredOptions - 1) (Model.toHighlighted model + 1)) model

        "ArrowUp" ->
            moveHighlight tagger (Basics.max 0 (Model.toHighlighted model - 1)) model

        "PageDown" ->
            moveHighlight tagger (Basics.min (List.length filteredOptions - 1) (Model.toHighlighted model + 10)) model

        "PageUp" ->
            moveHighlight tagger (Basics.max 0 (Model.toHighlighted model - 10)) model

        "Enter" ->
            case getAt (Model.toHighlighted model) filteredOptions of
                Just opt ->
                    ( Model.selectOption opt model, Effect.none )

                Nothing ->
                    ( Model.closeMenu model, Effect.none )

        "Escape" ->
            ( Model.closeMenu model, Effect.none )

        _ ->
            ( model, Effect.none )


moveHighlight : (Msg a -> msg) -> Int -> Model a -> ( Model a, Effect effect msg )
moveHighlight tagger newHighlighted model =
    if Model.isOpen model then
        ( Model.highlightIndex newHighlighted model
        , Effect.batch
            [ getContainerAndMenuElementsEffect tagger model
            , Effect.GetElementsAndScrollMenu
                (tagger NoOp)
                { menuId = Model.toMenuElementId model
                , optionId = Model.toOptionElementId model newHighlighted
                }
            ]
        )

    else
        ( model
        , getContainerAndMenuElementsEffect tagger model
        )


getContainerAndMenuElementsEffect : (Msg a -> msg) -> Model a -> Effect effect msg
getContainerAndMenuElementsEffect tagger model =
    Effect.GetContainerAndMenuElements
        (GotContainerAndMenuElements >> tagger)
        { menuId = Model.toMenuElementId model
        , containerId = Model.toContainerElementId model
        }


getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs
