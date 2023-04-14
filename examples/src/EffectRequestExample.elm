module EffectRequestExample exposing (Cocktail, Model, Msg(..), MyEffect(..), cocktailDecoder, init, main, update, view)

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
import Select.Effect


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init >> Tuple.mapSecond performEffect
        , view = view
        , update = \msg model -> update msg model |> Tuple.mapSecond performEffect
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { select : Select Cocktail
    }


type alias Cocktail =
    { id : String
    , name : String
    , imgUrl : String
    , instructions : String
    , ingredients : List String
    }


init : Decode.Value -> ( Model, MyEffect )
init flags =
    let
        selected =
            Decode.decodeValue flagDecoder flags

        ( select, effect ) =
            case selected of
                Ok sel ->
                    Select.init "cocktail-select"
                        |> Select.setInputValue sel.name
                        |> Select.Effect.sendRequest SelectMsg FetchCocktails (Just (\{ id } -> id == sel.id))
                        |> Tuple.mapSecond SelectEffect

                Err _ ->
                    ( Select.init "cocktail-select", NoEffect )
    in
    ( { select = select }
    , effect
    )


flagDecoder : Decoder { name : String, id : String }
flagDecoder =
    Decode.map2
        (\name id -> { name = name, id = id })
        (Decode.field "name" Decode.string)
        (Decode.field "id" Decode.string)



-- UPDATE


type Msg
    = SelectMsg (Select.Msg Cocktail)


update : Msg -> Model -> ( Model, MyEffect )
update msg model =
    case msg of
        SelectMsg subMsg ->
            Select.Effect.updateWith [ Select.Effect.request FetchCocktails ] SelectMsg subMsg model.select
                |> Tuple.mapBoth (\select -> { model | select = select }) SelectEffect


fetchCocktails : String -> (Result Http.Error (List Cocktail) -> Msg) -> Cmd Msg
fetchCocktails query tagger =
    Http.get
        { url = "https://thecocktaildb.com/api/json/v1/1/search.php?s=" ++ String.replace " " "+" query
        , expect =
            Http.expectJson tagger
                (Decode.field "drinks"
                    (Decode.oneOf
                        [ Decode.list cocktailDecoder
                        , Decode.succeed []
                        ]
                    )
                )
        }



-- EFFECT


type MyEffect
    = NoEffect
    | SelectEffect (Select.Effect MyEffect Msg)
    | FetchCocktails String (Result Http.Error (List Cocktail) -> Msg)


performEffect : MyEffect -> Cmd Msg
performEffect effect =
    case effect of
        NoEffect ->
            Cmd.none

        SelectEffect selectEffect ->
            Select.Effect.performWithRequest performEffect selectEffect

        FetchCocktails query msg ->
            fetchCocktails query msg



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
            [ Select.view
                |> Select.withClearButton (Just Resources.ClearButton.clearButton)
                |> Select.toElement []
                    { select = model.select
                    , onChange = SelectMsg
                    , label = Input.labelHidden "Find a cocktail"
                    , placeholder = Just (Input.placeholder [] (Element.text "Type to search cocktails"))
                    , itemToString = .name
                    }
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
    Decode.map5 Cocktail
        (Decode.field "idDrink" Decode.string)
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
