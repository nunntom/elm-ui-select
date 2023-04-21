module ElmCss.ExampleTest exposing (exampleProgramTest)

import Countries exposing (Country)
import ElmCss.EffectExample as App
import Expect
import Html
import Html.Attributes
import Html.Styled
import ProgramTest exposing (ProgramTest, SimulatedEffect)
import Select.Effect
import Select.ElmCss as Select exposing (Select)
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
                    |> focusInput
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
                    |> focusInput
                    |> ProgramTest.fillIn "" "Choose a country" "United"
                    |> Select.Effect.simulateClickOption simulateInputConfig "country-select" "🇬🇧 United Kingdom of Great Britain and Northern Ireland"
                    |> ProgramTest.expectViewHas [ Selector.text "You chose United Kingdom of Great Britain and Northern Ireland" ]
        , Test.test "Keyboard select United Kingdom" <|
            \() ->
                programTest
                    |> focusInput
                    |> ProgramTest.fillIn "" "Choose a country" "United"
                    |> SimulateInput.arrowDown "country-select"
                    |> SimulateInput.enter "country-select"
                    |> ProgramTest.expectViewHas [ Selector.text "You chose United Kingdom of Great Britain and Northern Ireland" ]
        , Test.test "Focusing on the input triggers the onFocus msg" <|
            \() ->
                programTest
                    |> focusInput
                    |> ProgramTest.expectModel (.inputIsFocused >> Expect.equal (Just True))
        , Test.test "Input losing focus triggers the onLoseFocus msg" <|
            \() ->
                programTest
                    |> focusInput
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
                    |> focusInput
                    |> ProgramTest.fillIn "" "Choose a country" "un"
                    |> ProgramTest.expectViewHasNot [ Selector.text "🇬🇧 United Kingdom of Great Britain and Northern Ireland" ]
        , Test.test "Typing 3 chars with withMinInputLength (Just 3) does show items" <|
            \() ->
                programTestWith (Select.withMinInputLength (Just 3))
                    |> focusInput
                    |> ProgramTest.fillIn "" "Choose a country" "uni"
                    |> ProgramTest.expectViewHas [ Selector.text "🇬🇧 United Kingdom of Great Britain and Northern Ireland" ]
        , Test.test "Typing less than minInputLength does not show no matches even if nothing matched" <|
            \() ->
                programTestWith (Select.withMinInputLength (Just 5))
                    |> focusInput
                    |> ProgramTest.fillIn "" "Choose a country" "zzzz"
                    |> ProgramTest.expectViewHasNot [ Selector.text "No matches" ]
        , Test.test "Typing up to the minInputLength shows no matches if nothing matched" <|
            \() ->
                programTestWith (Select.withMinInputLength (Just 3))
                    |> focusInput
                    |> ProgramTest.fillIn "" "Choose a country" "zzzz"
                    |> ProgramTest.expectViewHas [ Selector.text "No matches" ]
        , Test.test "Choosing an option and then focusing back on the input shows all the options again" <|
            \() ->
                programTest
                    |> focusInput
                    |> ProgramTest.fillIn "" "Choose a country" "United"
                    |> Select.Effect.simulateClickOption simulateInputConfig "country-select" "🇬🇧 United Kingdom of Great Britain and Northern Ireland"
                    |> ProgramTest.simulateDomEvent (Query.find [ Selector.id (Select.toInputElementId countrySelect) ]) Test.Html.Event.focus
                    |> ProgramTest.expectViewHas [ Selector.text "🇦🇩 Andorra" ]
        , Test.test "Setting open on focus to false does not open the menu when the input is focused" <|
            \() ->
                programTestWith (Select.withOpenMenuOnFocus False)
                    |> focusInput
                    |> ProgramTest.expectModel (.countrySelect >> Select.isMenuOpen >> Expect.equal False)
        , Test.test "Setting open on focus to true does open the menu when the input is focused" <|
            \() ->
                programTestWith (Select.withOpenMenuOnFocus True)
                    |> focusInput
                    |> ProgramTest.expectModel (.countrySelect >> Select.isMenuOpen >> Expect.equal True)
        , Test.test "Clicking clear button clears the input text" <|
            \() ->
                programTest
                    |> focusInput
                    |> ProgramTest.fillIn "" "Choose a country" "United"
                    |> ProgramTest.clickButton "clear"
                    |> ProgramTest.expectView (Query.find [ Selector.id (Select.toInputElementId countrySelect) ] >> Query.has [ Selector.attribute (Html.Attributes.value "") ])
        , Test.test "Clicking clear button clears a selected item and input text" <|
            \() ->
                programTest
                    |> focusInput
                    |> ProgramTest.fillIn "" "Choose a country" "United"
                    |> Select.Effect.simulateClickOption simulateInputConfig "country-select" "🇬🇧 United Kingdom of Great Britain and Northern Ireland"
                    |> ProgramTest.ensureViewHas [ Selector.text "You chose United Kingdom of Great Britain and Northern Ireland" ]
                    |> ProgramTest.ensureView (Query.find [ Selector.id (Select.toInputElementId countrySelect) ] >> Query.has [ Selector.attribute (Html.Attributes.value "🇬🇧 United Kingdom of Great Britain and Northern Ireland") ])
                    |> ProgramTest.clickButton "clear"
                    |> ProgramTest.expectViewHasNot [ Selector.text "You chose United Kingdom of Great Britain and Northern Ireland" ]
        ]


programTest : ProgramTest App.Model App.Msg App.MyEffect
programTest =
    ProgramTest.createElement
        { init = App.init
        , update = App.update
        , view = App.view >> Html.Styled.toUnstyled
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
                Html.div []
                    [ Html.label []
                        [ Html.text "Choose a country"
                        , Select.view
                            |> f
                            |> Select.toStyled []
                                { select = m.countrySelect
                                , onChange = App.CountrySelectMsg
                                , itemToString = \c -> c.flag ++ " " ++ c.name
                                }
                            |> Html.Styled.toUnstyled
                        ]
                    ]
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


focusInput : ProgramTest model msg effect -> ProgramTest model msg effect
focusInput =
    ProgramTest.simulateDomEvent (Query.find [ Selector.id (Select.toInputElementId countrySelect) ]) Test.Html.Event.focus
