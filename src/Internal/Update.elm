module Internal.Update exposing (update)

import Internal.Effect as Effect exposing (Effect)
import Internal.Model as Model exposing (Model)
import Internal.Msg exposing (Msg(..))
import Internal.Option exposing (Option)
import Internal.Request as Request exposing (Request)
import Internal.RequestState exposing (RequestState(..))


update : (Msg a -> msg) -> Maybe (Request effect) -> Msg a -> Model a -> ( Model a, Effect effect msg )
update tagger maybeRequest msg model =
    case msg of
        InputChanged val ->
            ( model
                |> Model.setInputValue val
                |> Model.highlightIndex 0
                |> Model.applyFilter True
                |> Model.setItems
                    (if maybeRequest /= Nothing && val == "" then
                        []

                     else
                        Model.toItems model
                    )
                |> Model.setRequestState
                    (if maybeRequest /= Nothing then
                        Just NotRequested

                     else
                        Nothing
                    )
            , case maybeRequest of
                Just req ->
                    if String.length val >= Request.toMinLength req then
                        Effect.Debounce (Request.toDelay req) (InputDebounceReturned val |> tagger)

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
            onFocusMenu tagger maybeRequest model

        InputClicked ->
            onFocusMenu tagger maybeRequest model

        InputLostFocus ->
            ( Model.setFocused False model
                |> Model.closeMenu
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
                    , menu = Maybe.map .container (Result.toMaybe result)
                    }
                |> Model.openMenu
            , Effect.none
            )

        ClearButtonPressed ->
            ( model
                |> Model.clear
                |> Model.setItems
                    (if maybeRequest == Nothing then
                        Model.toItems model

                     else
                        []
                    )
            , Effect.none
            )

        InputDebounceReturned val ->
            if val == Model.toInputValue model then
                ( Model.setRequestState (Just Loading) model
                , Maybe.map (Request.toEffect >> (\effect -> Effect.Request (effect val))) maybeRequest
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
                        , getContainerAndMenuElementsEffect tagger model
                        )

                    Err _ ->
                        ( Model.setRequestState (Just Failed) model
                        , Effect.none
                        )

            else
                ( model, Effect.none )

        NoOp ->
            ( model, Effect.none )


onFocusMenu : (Msg a -> msg) -> Maybe (Request effect) -> Model a -> ( Model a, Effect effect msg )
onFocusMenu tagger maybeRequest model =
    ( Model.setFocused True model
        |> Model.highlightIndex 0
    , if maybeRequest == Nothing || Model.toRequestState model == Just Success then
        getContainerAndMenuElementsEffect tagger model

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
            , Effect.GetElementsAndScrollMenu (\_ -> tagger NoOp)
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
    Effect.GetContainerAndMenuElements (GotContainerAndMenuElements >> tagger)
        { menuId = Model.toMenuElementId model
        , containerId = Model.toContainerElementId model
        }


getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs
