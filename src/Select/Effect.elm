module Select.Effect exposing
    ( Effect
    , update, updateWith
    , UpdateOption, request, requestMinInputLength, requestDebounceDelay, onSelectedChange, onInput, onFocus, onLoseFocus
    , perform, performWithRequest
    , simulate, simulateWithRequest
    , simulateClickOption, SimulateInputConfig
    , map, mapEffect
    )

{-| Update the Select by returning Effects instead of Cmds.
This module is designed to help testing with [elm-program-test](https://package.elm-lang.org/packages/avh4/elm-program-test/3.6.3/),
allowing you to simulate the effects produced by the select and simulate input. If you are not doing this kind of testing,
you don't need this module.


# Type

@docs Effect


# Update Effect

@docs update, updateWith


# Update Options

@docs UpdateOption, request, requestMinInputLength, requestDebounceDelay, onSelectedChange, onInput, onFocus, onLoseFocus


# Perform Effect

@docs perform, performWithRequest


# Simulating Effects

@docs simulate, simulateWithRequest


# Simulating Input

@docs simulateClickOption, SimulateInputConfig


# Mapping

@docs map, mapEffect

-}

import Html
import Html.Attributes
import Internal.Effect as Effect
import Internal.Model exposing (Model)
import Internal.Msg exposing (Msg)
import Internal.Update as Update
import Internal.UpdateOptions as UpdateOptions
import Json.Encode as Encode


{-| The Effect type
-}
type alias Effect effect msg =
    Effect.Effect effect msg



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
update : (Msg a -> msg) -> Msg a -> Select a -> ( Select a, Effect Never msg )
update =
    Update.update (UpdateOptions.fromList [])


{-| Update with options.

    update : Msg -> Model -> ( Model, MyEffect )
    update msg model =
        case msg of
            SelectMsg subMsg ->
                Select.Effect.updateWith [ Select.Effect.onSelectedChanged ThingSelected ] SelectMsg subMsg model.select
                    |> Tuple.mapFirst (\select -> { model | select = select })
                    |> Tuple.mapSecond SelectEffect

            ThingSelected maybeThing ->
                Debug.todo "Do something when the thing is selected/deselected"

-}
updateWith : List (UpdateOption err effect a msg) -> (Msg a -> msg) -> Msg a -> Select a -> ( Select a, Effect effect msg )
updateWith options =
    Update.update (UpdateOptions.fromList options)


{-| Options for use with updateWith.
-}
type alias UpdateOption err effect a msg =
    UpdateOptions.UpdateOption err effect a msg


{-| Update with an HTTP request to retrieve matching remote results.
Note that in order to avoid an elm/http dependency in this package, you will need to provide the request Effect yourself.

Provide an effect (your app's own Effect type) that uses the input value and a msg tagger that can perform an HTTP request.
Update will use this Effect when the user types into the input.

When the effect is performed you must use Select.Effect.performWithRequest instead of Select.Effect.perform.

    type MyEffect
        = SelectEffect (Select.Effect MyEffect Msg)
        | FetchThings String (Result Http.Error (List Thing) -> Msg)

    update : Msg -> Model -> ( Model, MyEffect )
    update msg model =
        case msg of
            SelectMsg subMsg ->
                Select.Effect.updateWith [ Select.Effect.request FetchThings ] SelectMsg subMsg model.select
                    |> Tuple.mapFirst (\select -> { model | select = select })
                    |> Tuple.mapSecond SelectEffect

    performEffect : MyEffect -> Cmd Msg
    performEffect effect =
        case effect of
            SelectEffect selectEffect ->
                Select.Effect.performWithRequest performEffect selectEffect

            FetchThings query tagger ->
                Http.get
                    { url = "https://awesome-thing.api/things?search=" ++ query
                    , expect =
                        Http.expectJson tagger
                            (Decode.list thingDecoder)
                    }

-}
request : (String -> (Result err (List a) -> msg) -> effect) -> UpdateOption err effect a msg
request effect =
    UpdateOptions.Request effect


{-| Configure debouncing for the request. How long should we wait in milliseconds after the user stops typing to send the request? Default is 300.
-}
requestDebounceDelay : Float -> UpdateOption err effect a msg
requestDebounceDelay delay =
    UpdateOptions.DebounceRequest delay


{-| How many characters does a user need to type before a request is sent?
If this is too low you may get an unmanagable number of results! Default is 3 characters.
-}
requestMinInputLength : Int -> UpdateOption err effect a msg
requestMinInputLength len =
    UpdateOptions.RequestMinInputLength len


{-| If provided this msg will be sent whenever the selected item changes.
-}
onSelectedChange : (Maybe a -> msg) -> UpdateOption err effect a msg
onSelectedChange msg =
    UpdateOptions.OnSelect msg


{-| If provided this msg will be sent whenever the input value changes.
-}
onInput : (String -> msg) -> UpdateOption err effect a msg
onInput msg =
    UpdateOptions.OnInput msg


{-| If provided this msg will be sent whenever the input gets focus.
-}
onFocus : msg -> UpdateOption err effect a msg
onFocus msg =
    UpdateOptions.OnFocus msg


{-| If provided this msg will be sent whenever the input loses focus.
-}
onLoseFocus : msg -> UpdateOption err effect a msg
onLoseFocus msg =
    UpdateOptions.OnLoseFocus msg



-- PERFORM


{-| Turn an Effect into a Cmd

    performEffect : MyEffect -> Cmd Msg
    performEffect effect =
        case effect of
            SelectEffect selectEffect ->
                Select.Effect.perform selectEffect

-}
perform : Effect Never msg -> Cmd msg
perform =
    Effect.perform (\_ -> Cmd.none)


{-| Perform the Effect with a request. You need to provide your own perform function to perform the provided request effect.

    performEffect : MyEffect -> Cmd Msg
    performEffect effect =
        case effect of
            SelectEffect selectEffect ->
                Select.Effect.performWithRequest performEffect selectEffect

            FetchThings query ->
                fetchThings (Select.gotRequestResponse >> SelectMsg) query

-}
performWithRequest : (effect -> Cmd msg) -> Effect effect msg -> Cmd msg
performWithRequest =
    Effect.perform


{-| Simulate the select effects. This is designed to work with [elm-program-test](https://package.elm-lang.org/packages/avh4/elm-program-test/3.6.3/), but since this package doesn't have it as a dependency,
you need to provide some of the functions to help with the simulation.

    simulateEffect : MyEffect -> SimulatedEffect Msg
    simulateEffect effect =
        case effect of
            SelectEffect selectEffect ->
                Select.Effect.simulate
                    Example.SelectMsg
                    { perform = SimulatedTask.perform
                    , batch = SimulatedCmd.batch
                    , sleep = SimulatedProcess.sleep
                    }
                    selectEffect

-}
simulate :
    { perform : (() -> msg) -> simulatedTask -> simulatedEffect
    , batch : List simulatedEffect -> simulatedEffect
    , sleep : Float -> simulatedTask
    }
    -> Effect Never msg
    -> simulatedEffect
simulate conf =
    Effect.simulate conf (\_ -> conf.batch [])


{-| Simulate the select effects with a request. This is designed to work with [elm-program-test](https://package.elm-lang.org/packages/avh4/elm-program-test/3.6.3/), but since this package doesn't have it as a dependency,
you need to provide some of the functions to help with the simulation.

    simulateEffect : MyEffect -> SimulatedEffect Msg
    simulateEffect effect =
        case effect of
            SelectEffect selectEffect ->
                Select.Effect.simulateWithRequest
                    Example.SelectMsg
                    { perform = SimulatedTask.perform
                    , batch = SimulatedCmd.batch
                    , sleep = SimulatedProcess.sleep
                    }
                    simulateEffect
                    selectEffect

            FetchThings query ->
                SimulateHttp.get
                    { url = "https://awesome-thing.api/things?search=" ++ query
                    , expect = SimulateHttp.expectJson tagger (Decode.list thingDecoder)
                    }

-}
simulateWithRequest :
    { perform : (() -> msg) -> simulatedTask -> simulatedEffect
    , batch : List simulatedEffect -> simulatedEffect
    , sleep : Float -> simulatedTask
    }
    -> (effect -> simulatedEffect)
    -> Effect effect msg
    -> simulatedEffect
simulateWithRequest =
    Effect.simulate


{-| Simulate clicking an option by the text label of the option. This is designed to help simulate input with elm-program-test.
Since this package doesn't have elm-test or elm-program-test as dependencies,
you need to provide some of the functions from those packages here.

    simulateConfig : Select.Effect.SimulateInputConfig (Single msg) Selector (ProgramTest model msg effect)
    simulateConfig =
        { simulateDomEvent = ProgramTest.simulateDomEvent
        , find = Query.find
        , attribute = Selector.attribute
        }

    selectTest : Test
    selectTest =
        Test.test "Typing United and clicking United Kingdom option selects United Kingdom" <|
            \() ->
                ProgramTest.createElement
                    { init = Example.init
                    , update = Example.update
                    , view = Example.view
                    }
                    |> ProgramTest.withSimulatedEffects simulateEffect
                    |> ProgramTest.start ()
                    |> ProgramTest.fillIn "" "Choose a country" "United Kingdom"
                    |> Select.Effect.simulateClickOption simulateConfig model.select "United Kingdom"
                    |> ProgramTest.expectViewHas [ Selector.text "You chose United Kingdom" ]

-}
simulateClickOption : SimulateInputConfig single selector programTest -> String -> String -> (programTest -> programTest)
simulateClickOption config id optionLabel =
    config.simulateDomEvent
        (config.find [ config.attribute (Html.Attributes.id (id ++ "-menu")) ]
            >> config.find
                [ config.attribute (Html.Attributes.attribute "role" "option")
                , config.attribute (Html.Attributes.attribute "value" optionLabel)
                ]
        )
        ( "click", Encode.object [] )


{-| Config type alias used by simulateClickOption
-}
type alias SimulateInputConfig single selector programTest =
    { simulateDomEvent : (single -> single) -> ( String, Encode.Value ) -> programTest -> programTest
    , find : List selector -> single -> single
    , attribute : Html.Attribute Never -> selector
    }



-- MAP


{-| Map Effect from one msg type to another
-}
map : (msg -> msg2) -> Effect effect msg -> Effect effect msg2
map =
    Effect.map


{-| Map Effect from one effect type to another
-}
mapEffect : (effect -> effect2) -> Effect effect msg -> Effect effect2 msg
mapEffect =
    Effect.mapEffect



-- INTERNAL


type alias Select a =
    Model a
