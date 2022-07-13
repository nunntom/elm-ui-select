module RequestTest exposing (exampleProgramTest)

import EffectRequestExample as App
import Http
import Json.Decode as Decode
import ProgramTest exposing (ProgramTest, SimulatedEffect)
import Select
import Select.Effect
import SimulatedEffect.Cmd as SimulatedCmd
import SimulatedEffect.Http as SimulateHttp
import SimulatedEffect.Process as SimulatedProcess
import SimulatedEffect.Task as SimulatedTask
import Test exposing (Test)
import Test.Html.Query as Query exposing (Single)
import Test.Html.Selector as Selector exposing (Selector)


exampleProgramTest : Test
exampleProgramTest =
    Test.describe "Select Tests"
        [ Test.test "Type in Chocolate, and choose second option with keyboard navigation" <|
            \() ->
                programTest
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chocolate"
                    |> ProgramTest.advanceTime 500
                    |> ProgramTest.simulateHttpOk "GET"
                        "https://thecocktaildb.com/api/json/v1/1/search.php?s=Chocolate"
                        cocktailsResponse
                    |> Select.Effect.simulateArrowDown simulateConfig "cocktail-select"
                    |> Select.Effect.simulateEnterKey simulateConfig "cocktail-select"
                    |> ProgramTest.expectViewHas [ Selector.text "Melt the bar in a small amount of boiling water. Add milk." ]
        , Test.test "Type in Chocolate, and choose \"Chocolate Drink\" with mouse click" <|
            \() ->
                programTest
                    |> ProgramTest.fillIn "" "Find a cocktail" "Chocolate"
                    |> ProgramTest.advanceTime 300
                    |> ProgramTest.simulateHttpOk "GET"
                        "https://thecocktaildb.com/api/json/v1/1/search.php?s=Chocolate"
                        cocktailsResponse
                    |> Select.Effect.simulateClickOption simulateConfig "cocktail-select" "Chocolate Drink"
                    |> ProgramTest.expectViewHas [ Selector.text "Melt the bar in a small amount of boiling water. Add milk." ]
        ]


model : App.Model
model =
    App.init ()
        |> Tuple.first


programTest : ProgramTest App.Model App.Msg App.MyEffect
programTest =
    ProgramTest.createElement
        { init = App.init
        , update = App.update
        , view = App.view
        }
        |> ProgramTest.withSimulatedEffects simulateEffect
        |> ProgramTest.start ()


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

        App.FetchCocktails query ->
            fetchCocktails (Select.gotRequestResponse query >> App.SelectMsg) query


fetchCocktails : (Result Http.Error (List App.Cocktail) -> msg) -> String -> SimulatedEffect msg
fetchCocktails tagger query =
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


cocktailsResponse : String
cocktailsResponse =
    """{
    "drinks": [
        {
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
