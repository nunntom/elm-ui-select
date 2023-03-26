module EffectExample exposing (Model, Msg(..), MyEffect(..), init, main, update, view)

{- This example shows how the select can be used with effects.
   And is useful for testing the elm-ui-select package as a whole.
-}

import Browser
import Countries exposing (Country)
import Element
import Element.Input as Input
import Html exposing (Html)
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
    Element.layout [] <|
        Element.column
            [ Element.centerX
            , Element.spacing 20
            , Element.padding 30
            ]
            [ Select.view
                |> Select.withClearButton (Just Resources.ClearButton.clearButton)
                |> Select.toElement []
                    { select = model.countrySelect
                    , onChange = CountrySelectMsg
                    , label = Input.labelAbove [] (Element.text "Choose a country")
                    , placeholder = Just (Input.placeholder [] (Element.text "Type to search"))
                    , itemToString = \c -> c.flag ++ " " ++ c.name
                    }
            , Maybe.map (\{ name } -> Element.text ("You chose " ++ name)) model.selectedCountry
                |> Maybe.withDefault Element.none
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
