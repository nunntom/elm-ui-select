module ExampleTest exposing (exampleProgramTest)

import Countries exposing (Country)
import EffectExample as App
import Element
import Element.Input as Input
import Expect
import Html
import ProgramTest exposing (ProgramTest, SimulatedEffect)
import Select exposing (Select)
import Select.Effect
import SimulateInput
import SimulatedEffect.Cmd as SimulatedCmd
import SimulatedEffect.Process as SimulatedProcess
import SimulatedEffect.Task as SimulatedTask
import Test exposing (Test)
import Test.Html.Event
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
                        (Query.find [ Selector.id (Select.toMenuElementId countrySelect) ]
                            >> Query.contains [ Html.text "🇬🇧 United Kingdom of Great Britain and Northern Ireland" ]
                        )
                    |> ProgramTest.expectView
                        (Query.find [ Selector.id (Select.toMenuElementId countrySelect) ]
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
        , Test.test "Focusing on the input triggers the onFocus msg" <|
            \() ->
                programTest
                    |> ProgramTest.simulateDomEvent (Query.find [ Selector.id (Select.toInputElementId countrySelect) ]) Test.Html.Event.focus
                    |> ProgramTest.expectModel (.inputIsFocused >> Expect.equal (Just True))
        , Test.test "Input losing focus triggers the onLoseFocus msg" <|
            \() ->
                programTest
                    |> ProgramTest.simulateDomEvent (Query.find [ Selector.id (Select.toInputElementId countrySelect) ]) Test.Html.Event.focus
                    |> ProgramTest.simulateDomEvent (Query.find [ Selector.id (Select.toInputElementId countrySelect) ]) Test.Html.Event.blur
                    |> ProgramTest.expectModel (.inputIsFocused >> Expect.equal (Just False))
        , Test.test "Filling in the input triggers the onInput msg" <|
            \() ->
                programTest
                    |> ProgramTest.fillIn "" "Choose a country" "Testing the input"
                    |> ProgramTest.expectModel (.inputValue >> Expect.equal "Testing the input")
        , Test.test "Typing 2 chars with withMinInputLength (Just 3) does not show any items" <|
            \() ->
                programTestWith (Select.withMinInputLength (Just 3))
                    |> ProgramTest.fillIn "" "Choose a country" "un"
                    |> ProgramTest.expectViewHasNot [ Selector.text "🇬🇧 United Kingdom of Great Britain and Northern Ireland" ]
        , Test.test "Typing 3 chars with withMinInputLength (Just 3) does shows items" <|
            \() ->
                programTestWith (Select.withMinInputLength (Just 3))
                    |> ProgramTest.fillIn "" "Choose a country" "uni"
                    |> ProgramTest.expectViewHas [ Selector.text "🇬🇧 United Kingdom of Great Britain and Northern Ireland" ]
        , Test.test "Typing less than minInputLength does not show no matches even if nothing matched" <|
            \() ->
                programTestWith (Select.withMinInputLength (Just 5))
                    |> ProgramTest.fillIn "" "Choose a country" "zzzz"
                    |> ProgramTest.expectViewHasNot [ Selector.text "No matches" ]
        , Test.test "Typing up to the minInputLength shows no matches if nothing matched" <|
            \() ->
                programTestWith (Select.withMinInputLength (Just 3))
                    |> ProgramTest.fillIn "" "Choose a country" "zzzz"
                    |> ProgramTest.expectViewHas [ Selector.text "No matches" ]
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


programTestWith : (Select.ViewConfig Country App.Msg -> Select.ViewConfig Country App.Msg) -> ProgramTest App.Model App.Msg App.MyEffect
programTestWith f =
    ProgramTest.createElement
        { init = App.init
        , update = App.update
        , view =
            \m ->
                Element.layout [] <|
                    (Select.view []
                        { onChange = App.CountrySelectMsg
                        , label = Input.labelAbove [] (Element.text "Choose a country")
                        , placeholder = Just (Input.placeholder [] (Element.text "Type to search"))
                        , itemToString = \c -> c.flag ++ " " ++ c.name
                        }
                        |> f
                        |> Select.toElement m.countrySelect
                    )
        }
        |> ProgramTest.withSimulatedEffects simulateEffect
        |> ProgramTest.start ()


countrySelect : Select Country
countrySelect =
    App.init ()
        |> Tuple.first
        |> .countrySelect


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
