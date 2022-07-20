module EffectExample exposing (Model, Msg(..), MyEffect(..), init, main, update, view)

import Browser
import Countries exposing (Country)
import Element
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Resources.ClearButton
import Select exposing (Select)
import Select.Effect


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init () |> Tuple.mapSecond performEffect
        , view = view
        , update = \msg model -> update msg model |> Tuple.mapSecond performEffect
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { countrySelect : Select Country
    }


init : () -> ( Model, MyEffect )
init _ =
    ( { countrySelect =
            Select.init "country-select"
                |> Select.setItems Countries.all
      }
    , NoEffect
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
                , label = Input.labelAbove [ Element.htmlAttribute <| Html.Attributes.for (Select.toInputElementId model.countrySelect) ] (Element.text "Choose a country")
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
    = SelectEffect (Select.Effect Never Msg)
    | NoEffect


update : Msg -> Model -> ( Model, MyEffect )
update msg model =
    case msg of
        CountrySelectMsg subMsg ->
            Select.Effect.update CountrySelectMsg subMsg model.countrySelect
                |> Tuple.mapFirst (\select -> { model | countrySelect = select })
                |> Tuple.mapSecond SelectEffect


performEffect : MyEffect -> Cmd Msg
performEffect effect =
    case effect of
        NoEffect ->
            Cmd.none

        SelectEffect selectEffect ->
            Select.Effect.perform selectEffect
