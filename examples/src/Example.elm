module Example exposing (main)

import Browser
import Countries exposing (Country)
import Element
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
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
            , Element.centerY
            , Element.spacing 20
            , Element.padding 30
            ]
            [ Element.html <|
                Html.node "meta"
                    [ Html.Attributes.name "viewport"
                    , Html.Attributes.attribute "content" "width=device-width, initial-scale=1"
                    ]
                    []
            , Select.view
                |> Select.withClearButton (Just Resources.ClearButton.clearButton)
                |> Select.toElement []
                    { select = model.countrySelect
                    , onChange = CountrySelectMsg
                    , label = Input.labelAbove [] (Element.text "Choose a country")
                    , placeholder = Just (Input.placeholder [] (Element.text "Type to search"))
                    , itemToString = \c -> c.flag ++ " " ++ c.name
                    }
            , Maybe.map (\{ name } -> Element.text ("You chose " ++ name)) (Select.toValue model.countrySelect)
                |> Maybe.withDefault Element.none
            ]


type Msg
    = CountrySelectMsg (Select.Msg Country)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CountrySelectMsg subMsg ->
            Select.update CountrySelectMsg subMsg model.countrySelect
                |> Tuple.mapFirst (\select -> { model | countrySelect = select })
