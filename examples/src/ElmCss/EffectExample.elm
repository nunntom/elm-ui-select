module ElmCss.EffectExample exposing (Model, Msg(..), MyEffect(..), init, main, update, view)

import Browser
import Countries exposing (Country)
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Select.Effect
import Select.ElmCss as Select exposing (Select)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init () |> Tuple.mapSecond performEffect
        , view = view >> Html.toUnstyled
        , update = \msg model -> update msg model |> Tuple.mapSecond performEffect
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { countrySelect : Select Country

    -- Note it is not necessary to store the following in the model, because you can just use `Select.toValue` etc
    -- These are just here for the tests
    , selectedCountry : Maybe Country
    , inputIsFocused : Maybe Bool
    , inputValue : String
    }


init : () -> ( Model, MyEffect )
init _ =
    ( { countrySelect =
            Select.init "country-select"
                |> Select.setItems Countries.all
      , selectedCountry = Nothing
      , inputIsFocused = Nothing
      , inputValue = ""
      }
    , NoEffect
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
        [ Html.label
            [ css
                [ Css.fontSize (Css.rem 1.2)
                , Css.lineHeight (Css.rem 1.5)
                ]
            ]
            [ Html.text "Choose a country"
            , Select.view
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
        , Html.div [ css [ Css.marginTop (Css.em 2) ] ]
            [ Maybe.map (\{ name } -> Html.text ("You chose " ++ name)) (Select.toValue model.countrySelect)
                |> Maybe.withDefault (Html.text "")
            ]
        ]


type Msg
    = CountrySelectMsg (Select.Msg Country)
    | SelectionChanged (Maybe Country)
    | InputFocused
    | InputLostFocus
    | InputChanged String


type MyEffect
    = SelectEffect (Select.Effect Never Msg)
    | NoEffect


update : Msg -> Model -> ( Model, MyEffect )
update msg model =
    case msg of
        CountrySelectMsg subMsg ->
            Select.Effect.updateWith
                [ Select.Effect.onSelectedChange SelectionChanged
                , Select.Effect.onFocus InputFocused
                , Select.Effect.onLoseFocus InputLostFocus
                , Select.Effect.onInput InputChanged
                ]
                CountrySelectMsg
                subMsg
                model.countrySelect
                |> Tuple.mapFirst (\select -> { model | countrySelect = select })
                |> Tuple.mapSecond SelectEffect

        SelectionChanged selected ->
            ( { model | selectedCountry = selected }, NoEffect )

        InputFocused ->
            ( { model | inputIsFocused = Just True }, NoEffect )

        InputLostFocus ->
            ( { model | inputIsFocused = Just False }, NoEffect )

        InputChanged val ->
            ( { model | inputValue = val }, NoEffect )


performEffect : MyEffect -> Cmd Msg
performEffect effect =
    case effect of
        NoEffect ->
            Cmd.none

        SelectEffect selectEffect ->
            Select.Effect.perform selectEffect
