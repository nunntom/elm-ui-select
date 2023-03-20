module RequestExample exposing (main)

{- This example shows how the select can be used to make requests based on the user input. -}

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder)
import Resources.ClearButton
import Select exposing (Select)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { select : Select Cocktail
    }


type alias Cocktail =
    { name : String
    , imgUrl : String
    , instructions : String
    , ingredients : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { select =
            Select.init "title-select"
                |> Select.setItems []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SelectMsg (Select.Msg Cocktail)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectMsg subMsg ->
            Select.updateWith [ Select.request fetchCocktails ] SelectMsg subMsg model.select
                |> Tuple.mapFirst (\select -> { model | select = select })


fetchCocktails : String -> (Result String (List Cocktail) -> msg) -> Cmd msg
fetchCocktails query tagger =
    Http.get
        { url = "https://thecocktaildb.com/api/json/v1/1/search.php?s=" ++ String.replace " " "+" query
        , expect =
            Http.expectJson (Result.mapError (\_ -> "Failed loading cocktails") >> tagger)
                (Decode.field "drinks"
                    (Decode.oneOf
                        [ Decode.list cocktailDecoder
                        , Decode.succeed []
                        ]
                    )
                )
        }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column
            [ Element.centerX
            , Element.paddingXY 0 100
            , Element.spacing 40
            , Element.width (Element.maximum 500 Element.shrink)
            ]
            [ Select.view []
                { onChange = SelectMsg
                , label = Input.labelHidden "Find a cocktail"
                , placeholder = Just (Input.placeholder [] (Element.text "Type to search cocktails"))
                , itemToString = .name
                }
                |> Select.withClearButton (Just Resources.ClearButton.clearButton)
                |> Select.withSelectExactMatchOnBlur True
                |> Select.toElement model.select
            , Maybe.map drinkView (Select.toValue model.select)
                |> Maybe.withDefault Element.none
            ]


drinkView : Cocktail -> Element Msg
drinkView cocktail =
    Element.column
        [ Element.spacing 30
        , Font.size 20
        , Background.color (Element.rgb 0.9 0.9 0.9)
        , Element.padding 40
        , Border.rounded 10
        ]
        [ Element.el
            [ Font.bold
            , Element.centerX
            , Font.size 24
            ]
            (Element.text cocktail.name)
        , Element.image [ Element.centerX, Element.height (Element.maximum 300 Element.fill) ] { src = cocktail.imgUrl, description = "" }
        , Element.el [ Font.bold ] (Element.text "Ingredients")
        , Element.column
            [ Element.spacing 5
            , Element.paddingXY 20 0
            , Font.size 16
            ]
            (List.map ((++) "• " >> Element.text) cocktail.ingredients)
        , Element.el [ Font.bold ] (Element.text "Instructions")
        , Element.paragraph [] [ Element.text cocktail.instructions ]
        ]



-- JSON


cocktailDecoder : Decoder Cocktail
cocktailDecoder =
    Decode.map4 Cocktail
        (Decode.field "strDrink" Decode.string)
        (Decode.field "strDrinkThumb" Decode.string)
        (Decode.field "strInstructions" Decode.string)
        decodeIngredients


decodeIngredients : Decoder (List String)
decodeIngredients =
    List.map
        (\i ->
            Decode.field ("strIngredient" ++ String.fromInt i) (Decode.maybe Decode.string)
        )
        (List.range 1 15)
        |> List.foldr (Decode.map2 (::)) (Decode.succeed [])
        |> Decode.map (List.filterMap identity >> List.filter ((/=) ""))
