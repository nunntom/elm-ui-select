module Internal.Update exposing (update)

import Internal.Effect as Effect exposing (Effect)
import Internal.Model as Model exposing (Model)
import Internal.Msg exposing (Msg(..))
import Internal.Option exposing (Option)
import Internal.Request as Request exposing (Request)
import Internal.RequestState exposing (RequestState(..))


update : Maybe (Request effect) -> Msg a -> Model a -> ( Model a, Effect effect )
update maybeRequest msg model =
    case msg of
        InputChanged val ->
            ( model
                |> Model.setInputValue val
                |> Model.highlightIndex 0
                |> Model.applyFilter True
                |> Model.setSelected Nothing
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
                        Effect.Debounce (Request.toDelay req) val

                    else
                        Effect.none

                Nothing ->
                    getContainerAndMenuElementsEffect model
            )

        OptionClicked opt ->
            ( Model.selectOption opt model
            , Effect.none
            )

        InputFocused ->
            onFocusMenu maybeRequest model

        InputClicked ->
            onFocusMenu maybeRequest model

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
            handleKey model key filteredOptions

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
                        , getContainerAndMenuElementsEffect model
                        )

                    Err _ ->
                        ( Model.setRequestState (Just Failed) model
                        , Effect.none
                        )

            else
                ( model, Effect.none )

        NoOp ->
            ( model, Effect.none )


onFocusMenu : Maybe (Request effect) -> Model a -> ( Model a, Effect effect )
onFocusMenu maybeRequest model =
    ( Model.setFocused True model
        |> Model.highlightIndex 0
    , if maybeRequest == Nothing || Model.toRequestState model == Just Success then
        getContainerAndMenuElementsEffect model

      else
        Effect.none
    )


handleKey : Model a -> String -> List (Option a) -> ( Model a, Effect effect )
handleKey model key filteredOptions =
    case key of
        "ArrowDown" ->
            moveHighlight (Basics.min (List.length filteredOptions - 1) (Model.toHighlighted model + 1)) model

        "ArrowUp" ->
            moveHighlight (Basics.max 0 (Model.toHighlighted model - 1)) model

        "PageDown" ->
            moveHighlight (Basics.min (List.length filteredOptions - 1) (Model.toHighlighted model + 10)) model

        "PageUp" ->
            moveHighlight (Basics.max 0 (Model.toHighlighted model - 10)) model

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


moveHighlight : Int -> Model a -> ( Model a, Effect effect )
moveHighlight newHighlighted model =
    if Model.isOpen model then
        ( Model.highlightIndex newHighlighted model
        , Effect.batch
            [ getContainerAndMenuElementsEffect model
            , Effect.GetElementsAndScrollMenu
                { menuId = Model.toMenuElementId model
                , optionId = Model.toOptionElementId model newHighlighted
                }
            ]
        )

    else
        ( model
        , getContainerAndMenuElementsEffect model
        )


getContainerAndMenuElementsEffect : Model a -> Effect effect
getContainerAndMenuElementsEffect model =
    Effect.GetContainerAndMenuElements
        { menuId = Model.toMenuElementId model
        , containerId = Model.toContainerElementId model
        }


getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs
