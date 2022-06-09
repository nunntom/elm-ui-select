module Example exposing (main)

import Browser
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
    Element.layout [] <|
        Element.column [ centerX, paddingXY 0 100 ]
            [ Select.view []
                { select = model.select
                , onChange = DropdownMsg
                , label = Input.labelAbove [] (text "My Select")
                , placeholder = Nothing
                , itemToString = \c -> c.flag ++ " " ++ c.name
                }
                |> Select.toElement
            ]


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
