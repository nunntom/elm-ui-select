module Effect exposing (main)

import Browser
import Countries exposing (Country)
import Element
import Element.Input as Input
import Html exposing (Html)
import Resources.ClearButton
import Select exposing (OptionState(..), Select)
import Select.Effect


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> update msg model |> Tuple.mapSecond performEffect
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { countrySelect : Select Country
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { countrySelect =
            Select.init "country-select"
                |> Select.setItems Countries.all
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column
            [ Element.centerX
            , Element.spacing 20
            , Element.padding 30
            ]
            [ Select.view []
                { onChange = CountrySelectMsg
                , label = Input.labelAbove [] (Element.text "Choose a country")
                , placeholder = Just (Input.placeholder [] (Element.text "Type to search"))
                , itemToString = \c -> c.flag ++ " " ++ c.name
                }
                |> Select.withClearButton (Just Resources.ClearButton.clearButton)
                |> Select.toElement model.countrySelect
            , Maybe.map (\{ name } -> Element.text ("You chose " ++ name)) (Select.toValue model.countrySelect)
                |> Maybe.withDefault Element.none
            ]


type Msg
    = CountrySelectMsg (Select.Msg Country)


type MyEffect
    = SelectEffect (Select.Effect Never)


update : Msg -> Model -> ( Model, MyEffect )
update msg model =
    case msg of
        CountrySelectMsg subMsg ->
            Select.Effect.update subMsg model.countrySelect
                |> Tuple.mapFirst (\select -> { model | countrySelect = select })
                |> Tuple.mapSecond SelectEffect


performEffect : MyEffect -> Cmd Msg
performEffect effect =
    case effect of
        SelectEffect selectEffect ->
            Select.Effect.perform CountrySelectMsg selectEffect
