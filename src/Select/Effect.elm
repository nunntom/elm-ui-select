module Select.Effect exposing
    ( Effect
    , update, updateWith, Request, request
    , perform, performWithRequest
    )

{-| Update the Select using the [Effects pattern](https://sporto.github.io/elm-patterns/architecture/effects.html)


# Type

@docs Effect


# Update Effect

@docs update, updateWith, Request, request


# Perform Effect

@docs perform, performWithRequest

-}

import Internal.Effect as Effect
import Internal.Model exposing (Model)
import Internal.Msg exposing (Msg)
import Internal.Request as Request
import Internal.Update as Update
import Select.UpdateConfig as UpdateConfig exposing (UpdateConfig)


{-| The Effect type
-}
type alias Effect effect =
    Effect.Effect effect



-- UPDATE


{-| Update the Select

    type MyEffect
        = SelectEffect (Select.Effect Never Msg)

    update : Msg -> Model -> ( Model, MyEffect )
    update msg model =
        case msg of
            SelectMsg subMsg ->
                Select.Effect.update SelectMsg subMsg model.select
                    |> Tuple.mapFirst (\select -> { model | select = select })
                    |> Tuple.mapSecond SelectEffect

    performEffect : MyEffect -> Cmd Msg
    performEffect effect =
        case effect of
            SelectEffect selectEffect ->
                Select.Effect.perform selectEffect

-}
update : Msg a -> Select a -> ( Select a, Effect Never )
update =
    Update.update UpdateConfig.default


{-| Update with an HTTP request. Note that in order to avoid an elm/http dependency in this package, you will need to provide the request Effect yourself.

    type MyEffect
        = SelectEffect (Select.Effect MyEffect Msg)
        | FetchThings String

    update : Msg -> Model -> ( Model, MyEffect )
    update msg model =
        case msg of
            SelectMsg subMsg ->
                Select.Effect.updateWithRequest SelectMsg (Select.Effect.request FetchThings) subMsg model.select
                    |> Tuple.mapFirst (\select -> { model | select = select })
                    |> Tuple.mapSecond SelectEffect

    performEffect : MyEffect -> Cmd Msg
    performEffect effect =
        case effect of
            SelectEffect selectEffect ->
                Select.Effect.performWithRequest performEffect selectEffect

            FetchThings query ->
                fetchThings (Select.gotRequestResponse query >> SelectMsg) query

    fetchThings : (Result Http.Error (List thing) -> msg) -> String -> Cmd msg
    fetchThings tagger query =
        Http.get
            { url = "https://awesome-thing.api/things?search=" ++ query
            , expect = Http.expectJson tagger (Decode.list thingDecoder)
            }

-}
updateWith : UpdateConfig effect -> Msg a -> Select a -> ( Select a, Effect effect )
updateWith =
    Update.update


{-| A request that uses your Effect type
-}
type alias Request effect =
    Request.Request effect


{-| Create a request. Provide a function that takes the input value and returns an Effect (your app's own Effect type)
that can be used to perform an HTTP request. Update will use this Effect when the user types in the input subject to a debounce delay
and minimum number of characters which can be configured in the [Request](Select-Request) module.
-}
request : (String -> effect) -> Request effect
request =
    Request.request



-- PERFORM


{-| Turn an Effect into a Cmd

    performEffect : MyEffect -> Cmd Msg
    performEffect effect =
        case effect of
            SelectEffect selectEffect ->
                Select.Effect.perform selectEffect

-}
perform : (Msg a -> msg) -> Effect Never -> Cmd msg
perform tagger =
    Effect.perform tagger (\_ -> Cmd.none)


{-| Perform the Effect with a request. You need to provide your own perform function to perform the provided request effect.

    performEffect : MyEffect -> Cmd Msg
    performEffect effect =
        case effect of
            SelectEffect selectEffect ->
                Select.Effect.performWithRequest performEffect selectEffect

            FetchThings query ->
                fetchThings (Select.gotRequestResponse >> SelectMsg) query

-}
performWithRequest : (Msg a -> msg) -> (effect -> Cmd msg) -> Effect effect -> Cmd msg
performWithRequest =
    Effect.perform



-- INTERNAL


type alias Select a =
    Model a
