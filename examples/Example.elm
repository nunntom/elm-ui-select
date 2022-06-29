module Example exposing (main)

import Browser
import ClearButton
import Countries exposing (Country)
import Element exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Select exposing (OptionState(..), Select)


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
            [ centerX
            , spacing 20
            , padding 30
            ]
            [ Select.view []
                { onChange = DropdownMsg
                , label = Input.labelAbove [] (text "Choose a country")
                , placeholder = Just (Input.placeholder [] (text "Type to search"))
                , itemToString = \c -> c.flag ++ " " ++ c.name
                }
                |> Select.withClearButton (Just ClearButton.clearButton)
                |> Select.toElement model.countrySelect
            , Maybe.map (\{ name } -> text ("You chose " ++ name)) (Select.toValue model.countrySelect)
                |> Maybe.withDefault Element.none
            ]


type Msg
    = DropdownMsg (Select.Msg Country)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DropdownMsg subMsg ->
            Select.update DropdownMsg subMsg model.countrySelect
                |> Tuple.mapFirst (\select -> { model | countrySelect = select })
