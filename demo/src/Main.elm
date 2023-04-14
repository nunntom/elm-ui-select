module Main exposing (main)

import Browser
import Countries exposing (Country)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes
import Html.Events
import Select exposing (Select)



-- MODEL


type alias Model =
    { select : Select Country
    , clearButton : Bool
    , forcePlacement : Maybe Select.MenuPlacement
    , moveDown : Maybe Int
    , maxWidth : Maybe Int
    , maxHeight : Maybe Int
    , minInputLength : Maybe Int
    , selectOnTab : Bool
    , customMenuStyle : Bool
    , placeholder : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { select =
            Select.init "country-select"
                |> Select.setItems Countries.all
      , clearButton = False
      , forcePlacement = Nothing
      , moveDown = Nothing
      , maxWidth = Nothing
      , maxHeight = Nothing
      , minInputLength = Nothing
      , selectOnTab = True
      , customMenuStyle = False
      , placeholder = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SelectMsg (Select.Msg Country)
    | ClearButtonChanged Bool
    | ForcePlacementChanged (Maybe Select.MenuPlacement)
    | MoveDownChanged (Maybe Int)
    | MaxWidthChanged (Maybe Int)
    | MaxHeightChanged (Maybe Int)
    | MinInputLengthChanged (Maybe Int)
    | SelectOnTabChanged Bool
    | CustomMenuStyleChanged Bool
    | PlaceholderChanged Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectMsg selectMsg ->
            Select.update SelectMsg selectMsg model.select
                |> Tuple.mapFirst (\s -> { model | select = s })

        ClearButtonChanged v ->
            ( { model | clearButton = v }, Cmd.none )

        ForcePlacementChanged v ->
            ( { model | forcePlacement = v }, Cmd.none )

        MoveDownChanged v ->
            ( { model | moveDown = v }, Cmd.none )

        MaxWidthChanged v ->
            ( { model | maxWidth = v }, Cmd.none )

        MaxHeightChanged v ->
            ( { model | maxHeight = v }, Cmd.none )

        MinInputLengthChanged v ->
            ( { model | minInputLength = v }, Cmd.none )

        SelectOnTabChanged v ->
            ( { model | selectOnTab = v }, Cmd.none )

        CustomMenuStyleChanged v ->
            ( { model | customMenuStyle = v }, Cmd.none )

        PlaceholderChanged v ->
            ( { model
                | placeholder =
                    if v then
                        Just "Start typing to search..."

                    else
                        Nothing
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Element.layout
        [ Element.padding 50
        , Element.height Element.fill
        ]
    <|
        Element.column
            [ Element.centerX
            , Element.spacing 50
            , Element.height Element.fill
            ]
            [ Element.paragraph
                [ Font.bold
                , Font.size 32
                , Font.center
                ]
                [ Element.text "elm-ui-select demo" ]
            , Element.row
                [ Element.spacing 50
                , Element.height Element.fill
                ]
                [ Select.view []
                    { onChange = SelectMsg
                    , itemToString = \c -> c.name ++ " " ++ c.flag
                    , label = Input.labelAbove [] (Element.text "Choose a country")
                    , placeholder = Maybe.map (Element.text >> Input.placeholder []) model.placeholder
                    }
                    |> configureIf model.clearButton (Select.withClearButton (Just clearButton))
                    |> configureIf (model.forcePlacement == Just Select.MenuAbove) Select.withMenuAlwaysAbove
                    |> configureIf (model.forcePlacement == Just Select.MenuBelow) Select.withMenuAlwaysBelow
                    |> Select.withMenuMaxHeight model.maxHeight
                    |> Select.withMenuMaxWidth model.maxWidth
                    |> Select.withMinInputLength model.minInputLength
                    |> Select.withSelectOnTab model.selectOnTab
                    |> configureIf model.customMenuStyle (Select.withMenuAttributes (menuAttributes <| Select.isMenuOpen model.select))
                    |> Select.toElement model.select
                    |> Element.el
                        [ Element.alignTop
                        , Element.htmlAttribute <| Html.Attributes.style "z-index" "100"
                        , Element.width <| Element.fillPortion 1
                        , Element.htmlAttribute <| Html.Attributes.style "position" "relative"
                        , Element.htmlAttribute <| Html.Attributes.style "top" (String.fromInt (Maybe.withDefault 0 model.moveDown) ++ "%")
                        ]
                , Element.column
                    [ Element.spacing 20
                    , Element.alignTop
                    , Element.width <| Element.fillPortion 1
                    , Font.size 16
                    , Background.gradient
                        { angle = 0.6
                        , steps =
                            [ Element.rgb255 241 238 255
                            , Element.rgb255 200 210 255
                            ]
                        }
                    , Element.padding 30
                    , Border.rounded 10
                    , Border.glow (Element.rgba 0 0 0 0.2) 2
                    ]
                    [ Input.checkbox []
                        { onChange = ClearButtonChanged
                        , icon = Input.defaultCheckbox
                        , checked = model.clearButton
                        , label = Input.labelRight [] (Element.text "With clear button")
                        }
                    , Input.checkbox []
                        { onChange = PlaceholderChanged
                        , icon = Input.defaultCheckbox
                        , checked = model.placeholder /= Nothing
                        , label = Input.labelRight [] (Element.text "Placeholder")
                        }
                    , Input.radio [ Element.spacing 10 ]
                        { onChange = ForcePlacementChanged
                        , options =
                            [ Input.option Nothing (Element.text "Auto")
                            , Input.option (Just Select.MenuAbove) (Element.text "Always above")
                            , Input.option (Just Select.MenuBelow) (Element.text "Always below")
                            ]
                        , selected = Just model.forcePlacement
                        , label =
                            Input.labelAbove [ Element.paddingEach { top = 0, bottom = 20, left = 0, right = 0 } ]
                                (Element.text "Placement of menu:")
                        }
                    , Element.column [ Element.spacing 20 ]
                        [ Element.text "Move down"
                        , Element.html <|
                            Html.input
                                [ Html.Attributes.type_ "range"
                                , Html.Attributes.min "0"
                                , Html.Attributes.max "90"
                                , Html.Attributes.style "appearance" "auto"
                                , Html.Attributes.style "-webkit-appearance" "auto"
                                , Html.Attributes.style "-moz-appearance" "auto"
                                , Html.Attributes.style "opacity" "1"
                                , Html.Attributes.style "position" "static"
                                , Html.Attributes.style "outline" "none"
                                , Html.Events.onInput (MoveDownChanged << String.toInt)
                                , Html.Attributes.value (String.fromInt (Maybe.withDefault 0 model.moveDown))
                                ]
                                []
                        , Element.paragraph
                            [ Font.size 10
                            , Font.center
                            , Font.alignLeft
                            ]
                            [ Element.text "Move the input down the page to see how it affects the placement and size of the dropdown menu." ]
                        ]
                    , Element.column
                        [ Element.spacing 10
                        , Element.width Element.fill
                        ]
                        [ Input.text []
                            { onChange = String.toInt >> MaxWidthChanged
                            , text = model.maxWidth |> Maybe.map String.fromInt |> Maybe.withDefault ""
                            , label = Input.labelAbove [] (Element.text "Max width of menu (pixels)")
                            , placeholder = Nothing
                            }
                        , Element.paragraph
                            [ Font.size 10
                            , Font.center
                            , Font.alignLeft
                            ]
                            [ Element.text """Limits the width of the dropdown menu, but only as far as the width of the input!""" ]
                        ]
                    , Input.text []
                        { onChange = String.toInt >> MaxHeightChanged
                        , text = model.maxHeight |> Maybe.map String.fromInt |> Maybe.withDefault ""
                        , label = Input.labelAbove [] (Element.text "Max height of menu (pixels)")
                        , placeholder = Nothing
                        }
                    , Input.text []
                        { onChange = String.toInt >> MinInputLengthChanged
                        , text = model.minInputLength |> Maybe.map String.fromInt |> Maybe.withDefault ""
                        , label = Input.labelAbove [] (Element.text "Min input length to show menu (characters)")
                        , placeholder = Nothing
                        }
                    , Input.checkbox []
                        { onChange = SelectOnTabChanged
                        , icon = Input.defaultCheckbox
                        , checked = model.selectOnTab
                        , label = Input.labelRight [] (Element.text "Select highlighted on tab")
                        }
                    , Input.checkbox []
                        { onChange = CustomMenuStyleChanged
                        , icon = Input.defaultCheckbox
                        , checked = model.customMenuStyle
                        , label = Input.labelRight [] (Element.text "Add some custom styles to the menu")
                        }
                    ]
                ]
            ]


configureIf : Bool -> (a -> a) -> a -> a
configureIf condition f =
    if condition then
        f

    else
        identity


clearButton : Select.ClearButton msg
clearButton =
    Select.clearButton [ Element.alignRight, Element.centerY, Element.moveLeft 12 ]
        (Element.el [ Font.size 10, Region.description "clear selection" ] (Element.text "❌"))


menuAttributes : Bool -> Select.MenuPlacement -> List (Element.Attribute msg)
menuAttributes isOpen placement =
    [ [ Background.color (Element.rgba 1 1 1 1)
      , Element.htmlAttribute (Html.Attributes.style "white-space" "auto")
      , Element.htmlAttribute (Html.Attributes.class "select-menu")
      , Element.htmlAttribute (Html.Attributes.style "transition" "transform 100ms ease-out, opacity 100ms ease-out")
      , Font.color (Element.rgba 0 0 0 1)
      , Border.rounded 2
      , Border.width 0
      , Border.shadow
            { offset = ( 0, 4 )
            , size = 3
            , blur = 10
            , color = Element.rgba 0 0 0 0.5
            }
      ]
    , if isOpen then
        [ Element.htmlAttribute (Html.Attributes.style "opacity" "1")
        ]

      else
        [ Element.htmlAttribute (Html.Attributes.style "opacity" "0")
        , Element.htmlAttribute (Html.Attributes.style "transform" "scale(0.95)")
        ]
    , case placement of
        Select.MenuBelow ->
            [ Element.moveDown 10 ]

        _ ->
            []
    ]
        |> List.concat



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
