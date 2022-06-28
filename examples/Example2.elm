module Example2 exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode as Decode exposing (Decoder)
import Select exposing (OptionState(..), Select)


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
    { select : Select CockTail
    }


type alias CockTail =
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
    = DropdownMsg (Select.Msg CockTail)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DropdownMsg subMsg ->
            let
                ( select, cmd ) =
                    Select.updateWithRequest (Select.request fetchCocktails) subMsg model.select
            in
            ( { model
                | select = select
              }
            , Cmd.map DropdownMsg cmd
            )


fetchCocktails : String -> Cmd (Select.Msg CockTail)
fetchCocktails query =
    Http.get
        { url = "https://thecocktaildb.com/api/json/v1/1/search.php?s=" ++ String.replace " " "+" query
        , expect = Http.expectJson Select.gotRequestResponse (Decode.field "drinks" (Decode.list cocktailDecoder))
        }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column
            [ centerX
            , paddingXY 0 100
            , spacing 40
            , width (maximum 500 shrink)
            ]
            [ Select.view []
                { onChange = DropdownMsg
                , label = Input.labelHidden "Find a cocktail"
                , placeholder = Just (Input.placeholder [] (text "Type to search cocktails"))
                , itemToString = .name
                }
                |> Select.withClearButton (Select.clearButton [ alignRight, centerY, moveLeft 12 ] (el [ Font.size 10, htmlAttribute (Html.Attributes.title "clear selection") ] (text "❌")))
                |> Select.toElement model.select
            , Maybe.map drinkView (Select.toValue model.select)
                |> Maybe.withDefault Element.none
            ]


drinkView : CockTail -> Element Msg
drinkView cocktail =
    column
        [ spacing 30
        , Font.size 20
        , Background.color (rgb 0.9 0.9 0.9)
        , padding 40
        , Border.rounded 10
        ]
        [ el
            [ Font.bold
            , centerX
            , Font.size 24
            ]
            (text cocktail.name)
        , image [ centerX, height (maximum 300 fill) ] { src = cocktail.imgUrl, description = "" }
        , el [ Font.bold ] (text "Ingredients")
        , column
            [ spacing 5
            , paddingXY 20 0
            , Font.size 16
            ]
            (List.map ((++) "• " >> text) cocktail.ingredients)
        , el [ Font.bold ] (text "Instructions")
        , paragraph [] [ text cocktail.instructions ]
        ]



-- JSON


cocktailDecoder : Decoder CockTail
cocktailDecoder =
    Decode.map4 CockTail
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
