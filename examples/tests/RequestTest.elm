module RequestTest exposing (exampleProgramTest)

import EffectRequestExample as App exposing (Cocktail)
import Expect
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import ProgramTest exposing (ProgramTest, SimulatedEffect)
import Select exposing (Select)
import Select.Effect
import SimulateInput
import SimulatedEffect.Cmd as SimulatedCmd
import SimulatedEffect.Http as SimulateHttp
import SimulatedEffect.Process as SimulatedProcess
import SimulatedEffect.Task as SimulatedTask
import Test exposing (Test)
import Test.Html.Event
import Test.Html.Query as Query exposing (Single)
import Test.Html.Selector as Selector exposing (Selector)


exampleProgramTest : Test
exampleProgramTest =
    Test.describe "Select Tests"
        [ Test.test "Type in Chocolate, and choose second option with keyboard navigation" <|
            \() ->
                programTest Nothing
                    |> focusInput
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chocolate"
                    |> ProgramTest.advanceTime 500
                    |> ProgramTest.simulateHttpOk "GET"
                        "https://thecocktaildb.com/api/json/v1/1/search.php?s=Chocolate"
                        cocktailsResponse
                    |> SimulateInput.arrowDown "cocktail-select"
                    |> SimulateInput.enter "cocktail-select"
                    |> ProgramTest.expectViewHas [ Selector.text "Melt the bar in a small amount of boiling water. Add milk." ]
        , Test.test "Type in Chocolate, and choose \"Chocolate Drink\" with mouse click" <|
            \() ->
                programTest Nothing
                    |> focusInput
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chocolate"
                    |> ProgramTest.advanceTime 300
                    |> ProgramTest.simulateHttpOk "GET"
                        "https://thecocktaildb.com/api/json/v1/1/search.php?s=Chocolate"
                        cocktailsResponse
                    |> Select.Effect.simulateClickOption simulateConfig "cocktail-select" "Chocolate Drink"
                    |> ProgramTest.expectViewHas [ Selector.text "Melt the bar in a small amount of boiling water. Add milk." ]
        , Test.test "Can initialise the select with an item selected" <|
            \() ->
                programTest (Just { name = "Chocolate Drink", id = "2" })
                    |> ProgramTest.simulateHttpOk "GET"
                        "https://thecocktaildb.com/api/json/v1/1/search.php?s=Chocolate+Drink"
                        cocktailsResponse
                    |> ProgramTest.expectViewHas [ Selector.text "Melt the bar in a small amount of boiling water. Add milk." ]
        , Test.test "Setting requestDebounceDelay to 0 sends a request at exactly requestMinInputLength and no more on further typing" <|
            \() ->
                programTestWithUpdateOptions
                    [ Select.Effect.request App.FetchCocktails
                    , Select.Effect.requestDebounceDelay 0
                    , Select.Effect.requestMinInputLength 3
                    ]
                    |> focusInput
                    |> ProgramTest.fillIn "" "Find a cocktail" "Cho"
                    |> ProgramTest.ensureHttpRequestWasMade "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Cho"
                    |> ProgramTest.simulateHttpOk "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Cho" cocktailsResponse
                    |> ProgramTest.fillIn "" "Find a cocktail" "Choc"
                    |> ProgramTest.advanceTime 500
                    |> ProgramTest.ensureHttpRequests "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Choc" (Expect.equal [])
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chocolate"
                    |> ProgramTest.advanceTime 500
                    |> ProgramTest.expectHttpRequests "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Chocolate" (Expect.equal [])
        , Test.test "Shows no match element when input value doesn't match any options after request" <|
            \() ->
                programTest Nothing
                    |> focusInput
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chocolatey"
                    |> ProgramTest.advanceTime 500
                    |> ProgramTest.simulateHttpOk "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Chocolatey" "{\"drinks\": []}"
                    |> ProgramTest.expectViewHas [ Selector.text "No matches" ]
        , Test.test "Doesn't show no match element when a new request is due to be made" <|
            \() ->
                programTest Nothing
                    |> focusInput
                    |> ProgramTest.fillIn "" "Find a cocktail" "Choc"
                    |> ProgramTest.advanceTime 500
                    |> ProgramTest.simulateHttpOk "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Choc" cocktailsResponse
                    |> ProgramTest.fillIn "" "Find a cocktail" "Choccy"
                    |> ProgramTest.ensureViewHasNot [ Selector.text "No matches" ]
                    |> ProgramTest.advanceTime 500
                    |> ProgramTest.expectViewHasNot [ Selector.text "No matches" ]
        , Test.test "Doesn't show no match element when input value doesn't match any options after request but input length is less than requestMinInputLength" <|
            \() ->
                programTestWithUpdateOptions
                    [ Select.Effect.request App.FetchCocktails
                    , Select.Effect.requestMinInputLength 3
                    ]
                    |> focusInput
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chocolatey"
                    |> ProgramTest.advanceTime 500
                    |> ProgramTest.simulateHttpOk "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Chocolatey" "{\"drinks\": []}"
                    |> ProgramTest.fillIn "" "Find a cocktail" "zz"
                    |> ProgramTest.expectViewHasNot [ Selector.text "No matches" ]
        , Test.test "Show no match element when debounce is 0, there are matching results returned but input is typed beyond requestMinInputLength and no longer matches" <|
            \() ->
                programTestWithUpdateOptions
                    [ Select.Effect.request App.FetchCocktails
                    , Select.Effect.requestDebounceDelay 0
                    , Select.Effect.requestMinInputLength 3
                    ]
                    |> focusInput
                    |> ProgramTest.fillIn "" "Find a cocktail" "Cho"
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chocolatey"
                    |> ProgramTest.simulateHttpOk "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Cho" cocktailsResponse
                    |> ProgramTest.expectViewHas [ Selector.text "No matches" ]
        , Test.test "Request is made if requestDebounceDelay is 0 and text longer than requestMinInputLength is entered in one (pasted) into the input" <|
            \() ->
                programTestWithUpdateOptions
                    [ Select.Effect.request App.FetchCocktails
                    , Select.Effect.requestDebounceDelay 0
                    , Select.Effect.requestMinInputLength 3
                    ]
                    |> focusInput
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chocolate"
                    |> ProgramTest.expectHttpRequestWasMade "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Chocolate"
        , Test.test "Doesn't show no matches if input is deleted shorter than min and entered as something that doesn't match prev results" <|
            \() ->
                programTestWithUpdateOptions
                    [ Select.Effect.request App.FetchCocktails
                    , Select.Effect.requestDebounceDelay 0
                    , Select.Effect.requestMinInputLength 3
                    ]
                    |> focusInput
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chocolate"
                    |> ProgramTest.simulateHttpOk "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Chocolate" cocktailsResponse
                    |> ProgramTest.fillIn "" "Find a cocktail" "zz"
                    |> ProgramTest.expectViewHasNot [ Selector.text "No matches" ]
        , Test.test "Doesn't show results if request is returned then input is deleted shorter than min request input length" <|
            \() ->
                programTestWithUpdateOptions
                    [ Select.Effect.request App.FetchCocktails
                    , Select.Effect.requestDebounceDelay 0
                    , Select.Effect.requestMinInputLength 3
                    ]
                    |> focusInput
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chocolate"
                    |> ProgramTest.simulateHttpOk "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Chocolate" cocktailsResponse
                    |> ProgramTest.ensureView
                        (Query.find [ Selector.id (Select.toMenuElementId drinkSelect) ]
                            >> Query.children []
                            >> Query.count (Expect.equal 2)
                        )
                    |> ProgramTest.fillIn "" "Find a cocktail" "Cho"
                    |> ProgramTest.ensureView
                        (Query.find [ Selector.id (Select.toMenuElementId drinkSelect) ]
                            >> Query.children []
                            >> Query.count (Expect.equal 2)
                        )
                    |> ProgramTest.fillIn "" "Find a cocktail" "Ch"
                    |> ProgramTest.expectView
                        (Query.find [ Selector.id (Select.toMenuElementId drinkSelect) ]
                            >> Query.children [ Selector.attribute <| Html.Attributes.attribute "role" "option" ]
                            >> Query.count (Expect.equal 0)
                        )
        , Test.test "Request is made if requestDebounceDelay is 0, text longer than min length is pasted, then different text is pasted that has the same first 3 characters as previous" <|
            \() ->
                programTestWithUpdateOptions
                    [ Select.Effect.request App.FetchCocktails
                    , Select.Effect.requestDebounceDelay 0
                    , Select.Effect.requestMinInputLength 3
                    ]
                    |> focusInput
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chocolate"
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chomp"
                    |> ProgramTest.expectHttpRequestWasMade "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Chomp"
        , Test.test "Shows items if request is made and more is typed in before request returns" <|
            \() ->
                programTest Nothing
                    |> focusInput
                    |> ProgramTest.fillIn "" "Find a cocktail" "Choc"
                    |> ProgramTest.advanceTime 500
                    |> ProgramTest.fillIn "" "Find a cocktail" "Choco"
                    |> ProgramTest.simulateHttpOk "GET" "https://thecocktaildb.com/api/json/v1/1/search.php?s=Choc" cocktailsResponse
                    |> ProgramTest.expectView
                        (Query.find [ Selector.id (Select.toMenuElementId drinkSelect) ]
                            >> Query.children [ Selector.attribute <| Html.Attributes.attribute "role" "option" ]
                            >> Query.count (Expect.atLeast 1)
                        )
        ]


programTest : Maybe { name : String, id : String } -> ProgramTest App.Model App.Msg App.MyEffect
programTest flags =
    ProgramTest.createElement
        { init = App.init
        , update = App.update
        , view = App.view
        }
        |> ProgramTest.withSimulatedEffects simulateEffect
        |> ProgramTest.start
            (Encode.object <|
                case flags of
                    Just { name, id } ->
                        [ ( "name", Encode.string name )
                        , ( "id", Encode.string id )
                        ]

                    Nothing ->
                        []
            )


programTestWithUpdateOptions : List (Select.Effect.UpdateOption err App.MyEffect Cocktail App.Msg) -> ProgramTest App.Model App.Msg App.MyEffect
programTestWithUpdateOptions updateOptions =
    ProgramTest.createElement
        { init = App.init
        , update =
            \msg model ->
                case msg of
                    App.SelectMsg subMsg ->
                        Select.Effect.updateWith updateOptions App.SelectMsg subMsg model.select
                            |> Tuple.mapBoth (\select -> { model | select = select }) App.SelectEffect
        , view = App.view
        }
        |> ProgramTest.withSimulatedEffects simulateEffect
        |> ProgramTest.start (Encode.object [])


simulateEffect : App.MyEffect -> SimulatedEffect App.Msg
simulateEffect effect =
    case effect of
        App.NoEffect ->
            SimulatedCmd.none

        App.SelectEffect selectEffect ->
            Select.Effect.simulateWithRequest
                { perform = SimulatedTask.perform
                , batch = SimulatedCmd.batch
                , sleep = SimulatedProcess.sleep
                }
                simulateEffect
                selectEffect

        App.FetchCocktails query msg ->
            fetchCocktails query msg


fetchCocktails : String -> (Result SimulateHttp.Error (List App.Cocktail) -> msg) -> SimulatedEffect msg
fetchCocktails query tagger =
    SimulateHttp.get
        { url = "https://thecocktaildb.com/api/json/v1/1/search.php?s=" ++ String.replace " " "+" query
        , expect =
            SimulateHttp.expectJson tagger
                (Decode.field "drinks"
                    (Decode.oneOf
                        [ Decode.list App.cocktailDecoder
                        , Decode.succeed []
                        ]
                    )
                )
        }


simulateConfig : Select.Effect.SimulateInputConfig (Single msg) Selector (ProgramTest model msg effect)
simulateConfig =
    { simulateDomEvent = ProgramTest.simulateDomEvent
    , find = Query.find
    , attribute = Selector.attribute
    }


drinkSelect : Select Cocktail
drinkSelect =
    App.init (Encode.object [])
        |> Tuple.first
        |> .select


focusInput : ProgramTest model msg effect -> ProgramTest model msg effect
focusInput =
    ProgramTest.simulateDomEvent (Query.find [ Selector.id (Select.toInputElementId drinkSelect) ]) Test.Html.Event.focus


cocktailsResponse : String
cocktailsResponse =
    """{
    "drinks": [
        {
            "idDrink": "1",
            "strDrink": "Chocolate Milk",
            "strInstructions": "Put the milk in the bottom, pour the Liquer on top and add the dash of amaretto. Do not mix. SLAM IT!",
            "strDrinkThumb": "https://www.thecocktaildb.com/images/media/drink/j6q35t1504889399.jpg",
            "strIngredient1": "Chocolate liqueur",
            "strIngredient2": "Milk",
            "strIngredient3": "Amaretto",
            "strIngredient4": null,
            "strIngredient5": null,
            "strIngredient6": null,
            "strIngredient7": null,
            "strIngredient8": null,
            "strIngredient9": null,
            "strIngredient10": null,
            "strIngredient11": null,
            "strIngredient12": null,
            "strIngredient13": null,
            "strIngredient14": null,
            "strIngredient15": null
        },
        {
            "idDrink": "2",
            "strDrink": "Chocolate Drink",
            "strInstructions": "Melt the bar in a small amount of boiling water. Add milk. Cook over low heat, whipping gently (with a whisk, i would assume) until heated well. Don't let it boil! Serve in coffee mug.",
            "strDrinkThumb": "https://www.thecocktaildb.com/images/media/drink/q7w4xu1487603180.jpg",
            "strIngredient1": "Chocolate",
            "strIngredient2": "Milk",
            "strIngredient3": "Water",
            "strIngredient4": null,
            "strIngredient5": null,
            "strIngredient6": null,
            "strIngredient7": null,
            "strIngredient8": null,
            "strIngredient9": null,
            "strIngredient10": null,
            "strIngredient11": null,
            "strIngredient12": null,
            "strIngredient13": null,
            "strIngredient14": null,
            "strIngredient15": null
        }
    ]
}"""
