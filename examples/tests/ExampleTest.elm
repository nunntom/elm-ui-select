module ExampleTest exposing (exampleProgramTest)

import EffectExample as App
import Expect
import Html
import ProgramTest exposing (ProgramTest, SimulatedEffect)
import Select
import Select.Effect
import SimulateInput
import SimulatedEffect.Cmd as SimulatedCmd
import SimulatedEffect.Process as SimulatedProcess
import SimulatedEffect.Task as SimulatedTask
import Test exposing (Test)
import Test.Html.Query as Query exposing (Single)
import Test.Html.Selector as Selector exposing (Selector)


exampleProgramTest : Test
exampleProgramTest =
    Test.describe "Select Tests"
        [ Test.test "Filter for United Kingdom produces one result" <|
            \() ->
                programTest
                    |> ProgramTest.fillIn "" "Choose a country" "United Kingdom"
                    |> ProgramTest.ensureView
                        (Query.find [ Selector.id (Select.toMenuElementId model.countrySelect) ]
                            >> Query.contains [ Html.text "🇬🇧 United Kingdom of Great Britain and Northern Ireland" ]
                        )
                    |> ProgramTest.expectView
                        (Query.find [ Selector.id (Select.toMenuElementId model.countrySelect) ]
                            >> Query.children []
                            >> Query.count (Expect.equal 1)
                        )
        , Test.test "Click United Kingdom selects it" <|
            \() ->
                programTest
                    |> ProgramTest.fillIn "" "Choose a country" "United"
                    |> Select.Effect.simulateClickOption simulateInputConfig "country-select" "🇬🇧 United Kingdom of Great Britain and Northern Ireland"
                    |> ProgramTest.expectViewHas [ Selector.text "You chose United Kingdom of Great Britain and Northern Ireland" ]
        , Test.test "Keyboard select United Kingdom" <|
            \() ->
                programTest
                    |> ProgramTest.fillIn "" "Choose a country" "United"
                    |> SimulateInput.arrowDown "country-select"
                    |> SimulateInput.enter "country-select"
                    |> ProgramTest.expectViewHas [ Selector.text "You chose United Kingdom of Great Britain and Northern Ireland" ]
        ]


programTest : ProgramTest App.Model App.Msg App.MyEffect
programTest =
    ProgramTest.createElement
        { init = App.init
        , update = App.update
        , view = App.view
        }
        |> ProgramTest.withSimulatedEffects simulateEffect
        |> ProgramTest.start ()


model : App.Model
model =
    App.init ()
        |> Tuple.first


simulateInputConfig : Select.Effect.SimulateInputConfig (Single msg) Selector (ProgramTest model msg effect)
simulateInputConfig =
    { simulateDomEvent = ProgramTest.simulateDomEvent
    , find = Query.find
    , attribute = Selector.attribute
    }


simulateEffect : App.MyEffect -> SimulatedEffect App.Msg
simulateEffect effect =
    case effect of
        App.NoEffect ->
            SimulatedCmd.none

        App.SelectEffect selectEffect ->
            Select.Effect.simulate
                { perform = SimulatedTask.perform
                , batch = SimulatedCmd.batch
                , sleep = SimulatedProcess.sleep
                }
                selectEffect
