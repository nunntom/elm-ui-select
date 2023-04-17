module Main exposing (main)

import Browser
import Countries exposing (Country)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Select exposing (Select)



-- MODEL


type alias Model =
    { countrySelect : Select Country
    , citySelect : Select String
    , whichSelect : WhichSelect
    , timeoutPassed : Bool
    , clearButton : Bool
    , forcePlacement : Maybe Select.MenuPlacement
    , moveDown : Int
    , maxWidth : Maybe Int
    , maxHeight : Maybe Int
    , minInputLength : Maybe Int
    , selectOnTab : Bool
    , customMenuStyle : Bool
    , placeholder : Maybe String
    , debounce : Int
    , requestMinChars : Int
    , showOptionsOnMobile : Bool
    }


type WhichSelect
    = CountrySelect
    | CitySelect


init : String -> ( Model, Cmd Msg )
init uniqueId =
    ( { countrySelect =
            Select.init ("country-select-" ++ uniqueId)
                |> Select.setItems Countries.all
      , citySelect = Select.init ("city-select-" ++ uniqueId)
      , whichSelect = CountrySelect
      , timeoutPassed = False
      , clearButton = False
      , forcePlacement = Nothing
      , moveDown = 0
      , maxWidth = Nothing
      , maxHeight = Nothing
      , minInputLength = Nothing
      , selectOnTab = True
      , customMenuStyle = False
      , placeholder = Nothing
      , debounce = 300
      , requestMinChars = 3
      , showOptionsOnMobile = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = CountrySelectMsg (Select.Msg Country)
    | CitySelectMsg (Select.Msg String)
    | WhichSelectChanged WhichSelect
    | ClearButtonChanged Bool
    | ForcePlacementChanged (Maybe Select.MenuPlacement)
    | MoveDownChanged Int
    | MaxWidthChanged (Maybe Int)
    | MaxHeightChanged (Maybe Int)
    | MinInputLengthChanged (Maybe Int)
    | SelectOnTabChanged Bool
    | CustomMenuStyleChanged Bool
    | PlaceholderChanged Bool
    | DebounceChanged Int
    | RequestMinCharsChanged Int
    | HamburgerPressed
    | CloseButtonPressed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CountrySelectMsg selectMsg ->
            Select.update CountrySelectMsg selectMsg model.countrySelect
                |> Tuple.mapFirst (\s -> { model | countrySelect = s })

        CitySelectMsg selectMsg ->
            Select.updateWith
                [ Select.request fetchCities
                , Select.requestDebounceDelay (toFloat model.debounce)
                ]
                CitySelectMsg
                selectMsg
                model.citySelect
                |> Tuple.mapFirst (\s -> { model | citySelect = s })

        WhichSelectChanged whichSelect ->
            ( { model | whichSelect = whichSelect }, Cmd.none )

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

        DebounceChanged v ->
            ( { model | debounce = v }, Cmd.none )

        RequestMinCharsChanged v ->
            ( { model | requestMinChars = v }, Cmd.none )

        HamburgerPressed ->
            ( { model | showOptionsOnMobile = True }, Cmd.none )

        CloseButtonPressed ->
            ( { model | showOptionsOnMobile = False }, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Element.layout
        [ Element.padding 50
        , Element.height Element.fill
        , Element.inFront <|
            Input.button
                [ Element.alignRight
                , Element.alignTop
                , Element.padding 20
                , Element.htmlAttribute <| Html.Attributes.class "responsive-mobile"
                ]
                { onPress = Just HamburgerPressed
                , label = hamburger
                }
        ]
    <|
        Element.column [ Element.height Element.fill ]
            [ Element.column
                [ Element.centerX
                , Element.spacing 50
                , Element.height Element.fill
                ]
                [ Element.paragraph
                    [ Font.bold
                    , Font.size 32
                    , Font.center
                    , Element.centerX
                    ]
                    [ Element.text "elm-ui-select demo" ]
                , Element.wrappedRow
                    [ Element.spacing 50
                    , Element.height Element.fill
                    ]
                    [ Element.column
                        [ Element.width Element.fill
                        , Element.spacing 20
                        , Element.alignTop
                        , Element.htmlAttribute <| Html.Attributes.style "position" "relative"
                        , Element.htmlAttribute <| Html.Attributes.style "top" (String.fromInt model.moveDown ++ "%")
                        ]
                        [ case model.whichSelect of
                            CountrySelect ->
                                selectView model
                                    "Choose a country"
                                    CountrySelectMsg
                                    (\c -> c.name ++ " " ++ c.flag)
                                    (\c -> Element.row [ Element.spacing 10 ] [ Element.text c.flag, Element.text c.name ])
                                    model.countrySelect

                            CitySelect ->
                                selectView model "Choose a city" CitySelectMsg identity Element.text model.citySelect
                        , Element.column
                            [ Element.width Element.fill
                            , Element.spacing 20
                            , Element.htmlAttribute <| Html.Attributes.style "transition" "opacity 0.5s"
                            , Element.htmlAttribute <|
                                Html.Attributes.style "opacity" <|
                                    if model.moveDown > 40 then
                                        "0"

                                    else
                                        "1"
                            , Element.htmlAttribute <|
                                Html.Attributes.style "height" <|
                                    if model.moveDown > 60 then
                                        "0px"

                                    else
                                        "auto"
                            ]
                            [ [ "Type to filter the options"
                              , "Up/down arrows navigate options (menu scrolls automatically)"
                              , "PgUp/PgDn moves highlighted option by 10"
                              , "Escape key closes the menu"
                              , "Try changing some of the configuration options"
                              ]
                                |> List.map
                                    (\t ->
                                        Element.row [ Element.spacing 10 ]
                                            [ Element.text "•"
                                            , Element.paragraph [] [ Element.text t ]
                                            ]
                                    )
                                |> Element.column
                                    [ Element.spacing 10
                                    , Font.size 14
                                    ]
                            , Element.row
                                [ Font.size 14
                                , Element.spacing 20
                                ]
                                [ Element.link [ Font.color (Element.rgb255 0 0 255) ]
                                    { url = "https://github.com/nunntom/elm-ui-select"
                                    , label = Element.text "View on GitHub"
                                    }
                                , Element.link [ Font.color (Element.rgb255 0 0 255) ]
                                    { url = "https://package.elm-lang.org/packages/nunntom/elm-ui-select/latest/"
                                    , label = Element.text "View docs"
                                    }
                                ]
                            ]
                        ]
                    , Element.el
                        [ Element.width <| Element.fillPortion 1
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
                        , Element.htmlAttribute <| Html.Attributes.class "responsive-desktop"
                        ]
                        (optionsView model)
                    ]
                ]
            , Element.el
                [ Element.paddingXY 20 50
                , Border.glow (Element.rgba 0 0 0 0.2) 5
                , Font.size 14
                , Element.htmlAttribute <| Html.Attributes.class "responsive-mobile"
                , Element.htmlAttribute <| Html.Attributes.style "position" "fixed"
                , Element.htmlAttribute <| Html.Attributes.style "top" "0"
                , Element.htmlAttribute <| Html.Attributes.style "right" "0"
                , Element.htmlAttribute <| Html.Attributes.style "height" "100%"
                , Element.htmlAttribute <| Html.Attributes.style "max-width" "100vw"
                , Background.color (Element.rgb255 255 255 255)
                , Element.htmlAttribute <|
                    Html.Attributes.style "transform" <|
                        if model.showOptionsOnMobile then
                            "translateX(0)"

                        else
                            "translateX(100%)"
                , Element.htmlAttribute <| Html.Attributes.style "transition" "transform 0.3s ease-in-out"
                , Element.htmlAttribute <| Html.Attributes.style "z-index" "1000"
                , Element.inFront <|
                    Input.button
                        [ Element.alignRight
                        , Element.alignTop
                        , Element.padding 20
                        ]
                        { onPress = Just CloseButtonPressed
                        , label = Element.el [ Font.size 40 ] (Element.text "✕")
                        }
                ]
                (optionsView model)
            ]


optionsView : Model -> Element Msg
optionsView model =
    Element.column
        [ Element.spacing 20
        , Element.width Element.fill
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
        , Element.column []
            [ Element.text "Move down"
            , range
                { min = "0"
                , max = "70"
                , step = "1"
                , value = String.fromInt model.moveDown
                , onChange = MoveDownChanged << Maybe.withDefault 0 << String.toInt
                }
            , Element.paragraph
                [ Font.size 10
                , Font.center
                , Font.alignLeft
                ]
                [ Element.text "Move the input down the page to see how it affects the placement and size of the dropdown menu." ]
            ]
        , Input.radioRow [ Element.spacing 10 ]
            { onChange = WhichSelectChanged
            , options =
                [ Input.option CountrySelect (Element.text "Countries")
                , Input.option CitySelect (Element.text "Cities (with api request)")
                ]
            , selected = Just model.whichSelect
            , label = Input.labelHidden "Select"
            }
        , case model.whichSelect of
            CountrySelect ->
                Element.none

            CitySelect ->
                Element.column
                    [ Element.spacing 20
                    , Element.width Element.fill
                    ]
                    [ Element.column [ Element.width Element.fill ]
                        [ Element.text <| "Debounce " ++ String.fromInt model.debounce ++ "ms"
                        , range
                            { min = "200"
                            , max = "1000"
                            , step = "50"
                            , onChange = String.toInt >> Maybe.withDefault 200 >> DebounceChanged
                            , value = String.fromInt model.debounce
                            }
                        ]
                    , Element.column [ Element.width Element.fill ]
                        [ Element.text <| "Require " ++ String.fromInt model.requestMinChars ++ " chars before sending request (debounced)"
                        , range
                            { min = "3"
                            , max = "10"
                            , step = "1"
                            , onChange = String.toInt >> Maybe.withDefault 0 >> RequestMinCharsChanged
                            , value = String.fromInt model.requestMinChars
                            }
                        ]
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
        , case model.whichSelect of
            CountrySelect ->
                Input.text []
                    { onChange = String.toInt >> MinInputLengthChanged
                    , text = model.minInputLength |> Maybe.map String.fromInt |> Maybe.withDefault ""
                    , label = Input.labelAbove [] (Element.text "Min input length to show menu (characters)")
                    , placeholder = Nothing
                    }

            CitySelect ->
                Element.none
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


selectView : Model -> String -> (Select.Msg a -> msg) -> (a -> String) -> (a -> Element msg) -> Select a -> Element msg
selectView model label tagger itemToString itemToElement select =
    Select.view
        |> configureIf model.clearButton (Select.withClearButton (Just clearButton))
        |> configureIf (model.forcePlacement == Just Select.MenuAbove) Select.withMenuAlwaysAbove
        |> configureIf (model.forcePlacement == Just Select.MenuBelow) Select.withMenuAlwaysBelow
        |> Select.withMenuMaxHeight model.maxHeight
        |> Select.withMenuMaxWidth model.maxWidth
        |> configureIf (model.whichSelect == CountrySelect) (Select.withMinInputLength model.minInputLength)
        |> Select.withSelectOnTab model.selectOnTab
        |> configureIf model.customMenuStyle
            (Select.withMenuAttributes (menuAttributes <| Select.isMenuOpen select)
                >> Select.withOptionElement (optionElement itemToElement)
            )
        |> Select.toElement []
            { select = select
            , onChange = tagger
            , itemToString = itemToString
            , label = Input.labelAbove [] (Element.text label)
            , placeholder = Maybe.map (Element.text >> Input.placeholder []) model.placeholder
            }
        |> Element.el
            [ Element.alignTop
            , Element.htmlAttribute <| Html.Attributes.style "z-index" "100"
            , Element.width <| Element.fillPortion 1
            ]


configureIf : Bool -> (a -> a) -> a -> a
configureIf condition f =
    if condition then
        f

    else
        identity


range :
    { min : String
    , max : String
    , step : String
    , value : String
    , onChange : String -> msg
    }
    -> Element msg
range { min, max, step, value, onChange } =
    Element.el [ Element.width Element.fill ] <|
        Element.html <|
            Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min min
                , Html.Attributes.max max
                , Html.Attributes.step step
                , Html.Attributes.style "appearance" "auto"
                , Html.Attributes.style "-webkit-appearance" "auto"
                , Html.Attributes.style "-moz-appearance" "auto"
                , Html.Attributes.style "opacity" "1"
                , Html.Attributes.style "position" "static"
                , Html.Attributes.style "outline" "none"
                , Html.Events.onInput onChange
                , Html.Attributes.value value
                ]
                []


clearButton : Select.ClearButton msg
clearButton =
    Select.clearButton [ Element.alignRight, Element.centerY, Element.moveLeft 12 ]
        (Element.el [ Font.size 10, Region.description "clear selection" ] (Element.text "❌"))


menuAttributes : Bool -> Select.MenuPlacement -> List (Element.Attribute msg)
menuAttributes isOpen placement =
    [ [ Background.color (Element.rgba 1 1 1 1)
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


optionElement : (a -> Element msg) -> Select.OptionState -> a -> Element msg
optionElement itemToElement state a =
    Element.el
        [ Element.pointer
        , Element.width Element.fill
        , Element.paddingXY 14 10
        , Background.color <|
            case state of
                Select.Highlighted ->
                    Element.rgb 0.89 0.89 0.89

                Select.Selected ->
                    Element.rgba 0.64 0.83 0.97 0.8

                Select.SelectedAndHighlighted ->
                    Element.rgba 0.64 0.83 0.97 1

                Select.Idle ->
                    Element.rgb 1 1 1
        ]
        (itemToElement a)


hamburger : Element msg
hamburger =
    [ 1, 2, 3 ]
        |> List.map
            (\_ ->
                Element.el
                    [ Element.height (Element.px 4)
                    , Element.width (Element.px 35)
                    , Background.color (Element.rgba 0 0 0 1)
                    ]
                    Element.none
            )
        |> Element.column [ Element.spacing 6 ]



-- HTTP


fetchCities : String -> (Result Http.Error (List String) -> msg) -> Cmd msg
fetchCities query tagger =
    Http.get
        { url = "https://api.teleport.org/api/cities/?search=" ++ query
        , expect = Http.expectJson tagger decodeCities
        }


decodeCities : Decoder (List String)
decodeCities =
    Decode.at [ "_embedded", "city:search-results" ]
        (Decode.list (Decode.field "matching_full_name" Decode.string))



-- http://geodb-free-service.wirefreethought.com/v1/geo/cities?namePrefix=oxford
-- MAIN


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
