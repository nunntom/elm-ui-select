module Select.Effect exposing
    ( Effect
    , update, updateWithRequest, Request, request
    , perform, performWithRequest
    , simulate, simulateWithRequest
    , SimulateInputConfig, simulateArrowDown, simulateArrowUp, simulateClickOption, simulateEnterKey
    , map, mapEffect
    )

{-| Update the Select by returning Effects instead of Cmds.
This module is designed to help testing with [elm-program-test](https://package.elm-lang.org/packages/avh4/elm-program-test/3.6.3/),
allowing you to simulate the effects produced by the select and simulate input. If you are not doing this kind of testing,
you don't need this module.


# Type

@docs Effect


# Update Effect

@docs update, updateWithRequest, Request, request


# Perform Effect

@docs perform, performWithRequest


# Simulating Effects

@docs simulate, simulateWithRequest


# Simulating Input

@docs SimulateInputConfig, simulateArrowDown, simulateArrowUp, simulateClickOption, simulateEnterKey


# Mapping

@docs map, mapEffect

-}

import Html
import Html.Attributes
import Internal.Effect as Effect
import Internal.Model exposing (Model)
import Internal.Msg exposing (Msg)
import Internal.Request as Request
import Internal.Update as Update
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
    Update.update Nothing


{-| Update with an HTTP request to retrieve matching remote results.
Note that in order to avoid an elm/http dependency in this package, you will need to provide the request Effect yourself.

    type MyEffect
        = SelectEffect (Select.Effect MyEffect Msg)
        | FetchThings String

    update : Msg -> Model -> ( Model, MyEffect )
    update msg model =
        case msg of
            SelectMsg subMsg ->
                Select.Effect.updateWithRequest (Select.request fetchThings) SelectMsg subMsg model.select
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
updateWithRequest : Request effect -> (Msg a -> msg) -> Msg a -> Select a -> ( Select a, Effect effect msg )
updateWithRequest req =
    Update.update (Just req)


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


{-| Simulate input. This is designed to help simulate input with elm-program-test.
Since this package doesn't have elm-test or elm-program-test as dependencies,
you need to provide some of the functions from those packages here.

    simulateConfig : Select.Effect.SimulateInputConfig (Single msg) Selector (ProgramTest model msg effect)
    simulateConfig =
        { simulateDomEvent = ProgramTest.simulateDomEvent
        , find = Query.find
        , attribute = Selector.attribute
        , containing = Selector.containing
        , text = Selector.text
        }

    selectTest : Test
    selectTest =
        Test.test "Type in United and choose United Kingdom with a mouse click" <|
            \() ->
                ProgramTest.createElement
                    { init = Example.init
                    , update = Example.update
                    , view = Example.view
                    }
                    |> ProgramTest.withSimulatedEffects simulateEffect
                    |> ProgramTest.start ()
                    |> Select.Effect.simulateFillIn simulateConfig model.select "United"
                    |> Select.Effect.simulateClickOption simulateConfig model.select "United Kingdom"
                    |> ProgramTest.expectViewHas [ Selector.text "You chose United Kingdom" ]

-}
type alias SimulateInputConfig single selector programTest =
    { simulateDomEvent : (single -> single) -> ( String, Encode.Value ) -> programTest -> programTest
    , find : List selector -> single -> single
    , attribute : Html.Attribute Never -> selector
    }


{-| Simulate pressing the arrow down key in the input
-}
simulateArrowDown : SimulateInputConfig single selector programTest -> String -> (programTest -> programTest)
simulateArrowDown =
    simulateKey "ArrowDown"


{-| Simulate pressing the arrow up key in the input
-}
simulateArrowUp : SimulateInputConfig single selector programTest -> String -> (programTest -> programTest)
simulateArrowUp =
    simulateKey "ArrowUp"


{-| Simulate pressing the enter key in the input
-}
simulateEnterKey : SimulateInputConfig single selector programTest -> String -> (programTest -> programTest)
simulateEnterKey =
    simulateKey "Enter"


{-| Simulate clicking an option by the text label of the option.
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


simulateKey : String -> SimulateInputConfig single selector programTest -> String -> (programTest -> programTest)
simulateKey key config id =
    config.simulateDomEvent (config.find [ config.attribute (Html.Attributes.id (id ++ "-input")) ])
        ( "keydown", Encode.object [ ( "key", Encode.string key ) ] )
