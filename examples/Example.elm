module Example exposing (main)

import Browser
import Countries exposing (Country)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
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
    { select : Select Country
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { select =
            Select.init "title-select"
                |> Select.setItems Countries.all
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.inFront <|
            Element.column
                [ centerX
                , paddingXY 0 100
                , spacing 10
                , width (px 500)
                , height (px 300)
                , Background.color (rgb 0.8 0.8 0.8)
                , padding 20
                , Element.htmlAttribute (Html.Attributes.style "top" "50px")
                , scrollbarY
                ]
                [ Select.view []
                    { onChange = DropdownMsg
                    , label = Input.labelAbove [] (text "Choose one")
                    , placeholder = Just (Input.placeholder [] (text "Type to search"))
                    , itemToString = \c -> c.flag ++ " " ++ c.name
                    }
                    |> Select.withFixedPosition True
                    |> Select.withClearButton (Select.clearButton [ alignRight, centerY, moveLeft 12 ] (el [ Font.size 10, htmlAttribute (Html.Attributes.title "clear selection") ] (text "❌")))
                    |> Select.toElement model.select
                , Maybe.map (\{ name } -> text ("You chose " ++ name)) (Select.toValue model.select)
                    |> Maybe.withDefault Element.none
                ]
        ]
        Element.none


type Msg
    = DropdownMsg (Select.Msg Country)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DropdownMsg subMsg ->
            let
                ( select, cmd ) =
                    Select.update subMsg model.select
            in
            ( { model
                | select = select
              }
            , Cmd.map DropdownMsg cmd
            )
