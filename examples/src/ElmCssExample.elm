module ElmCssExample exposing (main)

import Browser
import Countries exposing (Country)
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr exposing (css)
import Select.ElmCss as Select exposing (Select)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
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
    Html.div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.flexDirection Css.column
            , Css.marginTop (Css.px 200)
            , Css.property "gap" "2em"
            , Css.fontFamilies [ "Arial" ]
            ]
        ]
        [ Html.node "meta"
            [ Attr.name "viewport"
            , Attr.attribute "content" "width=device-width, initial-scale=1"
            ]
            []

        --<meta name="viewport" content="width=device-width, initial-scale=1" />
        , Html.label
            [ css
                [ Css.fontSize (Css.rem 1.2)
                , Css.lineHeight (Css.rem 1.5)
                ]
            ]
            [ Select.view
                |> Select.withElementBefore (Just <| Html.text "Choose a country")
                |> Select.withClearButton
                    (Just <|
                        Select.clearButton
                            [ Css.height (Css.pct 100)
                            , Css.displayFlex
                            , Css.alignItems Css.center
                            , Css.marginRight (Css.em 1)
                            , Css.fontSize (Css.rem 0.6)
                            , Css.cursor Css.pointer
                            ]
                            (Html.text "❌")
                    )
                |> Select.toStyled
                    [ Css.padding (Css.em 0.5)
                    , Css.paddingRight (Css.em 1.5)
                    , Css.fontSize (Css.rem 1.2)
                    , Css.borderRadius (Css.px 4)
                    , Css.borderWidth (Css.px 1)
                    , Css.borderColor (Css.rgba 0 0 0 0.5)
                    ]
                    { select = model.countrySelect
                    , onChange = CountrySelectMsg
                    , itemToString = \c -> c.flag ++ " " ++ c.name
                    }
            ]
        , Html.div []
            [ Maybe.map (\{ name } -> Html.text ("You chose " ++ name)) (Select.toValue model.countrySelect)
                |> Maybe.withDefault (Html.text "")
            ]
        ]


type Msg
    = CountrySelectMsg (Select.Msg Country)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CountrySelectMsg subMsg ->
            Select.update CountrySelectMsg subMsg model.countrySelect
                |> Tuple.mapFirst (\select -> { model | countrySelect = select })
