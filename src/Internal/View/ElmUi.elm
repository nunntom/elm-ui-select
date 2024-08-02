module Internal.View.ElmUi exposing
    ( Config
    , ViewConfig
    , defaultOptionElement
    , init
    , toElement
    )

import Browser.Dom as Dom
import Html.Attributes
import Html.Events
import Internal.Model as Model exposing (Model)
import Internal.Msg exposing (Msg(..))
import Internal.Option as InternalOption
import Internal.OptionState exposing (OptionState(..))
import Internal.Placement as Placement exposing (Placement)
import Internal.View.Common as View
import Internal.View.Events as ViewEvents
import Internal.ViewConfig as ViewConfig exposing (ViewConfigInternal)
import Json.Decode as Decode
import Ui exposing (Attribute, Element, alignRight, alignTop, background, below, border, borderColor, clipY, column, down, el, fill, height, heightMax, html, inFront, move, none, padding, paddingXY, pointer, px, rgb, rgba, rounded, row, shrink, text, up, width, widthMax, widthMin)
import Ui.Accessibility as Region
import Ui.Events as Events
import Ui.Font as Font
import Ui.Input as Input
import Ui.Prose as Prose


type alias Config a msg =
    { select : Model a
    , onChange : Msg a -> msg
    , itemToString : a -> String
    , label : Input.Label
    , placeholder : Maybe String
    }


type alias ViewConfig a msg =
    ViewConfigInternal a (Attribute msg) (Element msg)


init : ViewConfigInternal a (Attribute msg) (Element msg)
init =
    ViewConfig.init


toElement : List (Attribute msg) -> Config a msg -> ViewConfig a msg -> Element msg
toElement attrs ({ select } as config) viewConfig =
    if Model.isMobile select then
        mobileView attrs (ViewConfig.toFilteredOptions select config.itemToString viewConfig) config viewConfig

    else
        toElement_ attrs
            (ViewConfig.toPlacement select viewConfig)
            (ViewConfig.toFilteredOptions select config.itemToString viewConfig)
            config
            viewConfig


toElement_ : List (Attribute msg) -> Placement -> List (InternalOption.Option a) -> Config a msg -> ViewConfig a msg -> Element msg
toElement_ attrs placement filteredOptions ({ select } as config) viewConfig =
    el
        (List.concat
            [ [ Ui.htmlAttribute (Html.Attributes.id <| Model.toContainerElementId select)
              , Ui.htmlAttribute (Html.Attributes.class "elm-select-container")
              , View.relativeContainerMarker config.onChange config.itemToString select viewConfig filteredOptions
                    |> html
                    |> inFront
              , width fill
              , below <|
                    if ViewConfig.shouldShowNoMatchElement filteredOptions select viewConfig then
                        Maybe.withDefault defaultNoMatchElement viewConfig.noMatchElement

                    else
                        none
              , Placement.toAttribute
                    (if viewConfig.positionFixed then
                        Placement.Below

                     else
                        placement
                    )
                <|
                    (if viewConfig.positionFixed then
                        positionFixedEl placement (Model.toContainerElement select)

                     else
                        identity
                    )
                    <|
                        menuView
                            (defaultMenuAttrs placement
                                { menuWidth = Model.toMenuMinWidth select
                                , maxWidth = viewConfig.menuMaxWidth
                                , menuHeight = Model.toMenuMaxHeight viewConfig.menuMaxHeight viewConfig.menuPlacement select
                                }
                                ++ List.concatMap (\toAttrs -> toAttrs placement) viewConfig.menuAttributes
                            )
                            { menuId = Model.toMenuElementId select
                            , toOptionId = Model.toOptionElementId select
                            , toOptionState = Model.toOptionState select
                            , onChange = config.onChange
                            , menuOpen = Model.isOpen select
                            , options = filteredOptions
                            , optionElement = Maybe.withDefault (defaultOptionElement config.itemToString) viewConfig.optionElement
                            , closeOnSelect = viewConfig.closeOnSelect
                            }
              ]
            , if Model.isOpen select then
                [ Ui.htmlAttribute <| Html.Attributes.style "z-index" "21" ]

              else
                []
            , ViewEvents.updateFilteredOptions config.onChange config.itemToString select viewConfig filteredOptions
                |> List.map Ui.htmlAttribute
            ]
        )
        (inputView attrs filteredOptions config viewConfig)


mobileView : List (Attribute msg) -> List (InternalOption.Option a) -> Config a msg -> ViewConfig a msg -> Element msg
mobileView attrs filteredOptions ({ select } as config) viewConfig =
    column
        ([ Ui.htmlAttribute (Html.Attributes.id <| Model.toContainerElementId select)
         , Ui.htmlAttribute (Html.Attributes.class "elm-select-container")
         , Ui.htmlAttribute (Html.Attributes.attribute "data-mobile" "true")
         , width fill
         , View.relativeContainerMarker config.onChange config.itemToString select viewConfig filteredOptions
            |> html
            |> inFront
         ]
            ++ (ViewEvents.updateFilteredOptions config.onChange config.itemToString select viewConfig filteredOptions
                    |> List.map Ui.htmlAttribute
               )
            ++ (if Model.isOpen select then
                    [ Ui.htmlAttribute (Html.Attributes.style "position" "fixed")
                    , Ui.htmlAttribute (Html.Attributes.style "top" "0")
                    , Ui.htmlAttribute (Html.Attributes.style "left" "0")
                    , Ui.htmlAttribute (Html.Attributes.style "right" "0")
                    , Ui.htmlAttribute (Html.Attributes.style "bottom" "0")
                    , Ui.htmlAttribute (Html.Attributes.style "height" "100%")
                    , Ui.htmlAttribute (Html.Attributes.style "z-index" "100")
                    , Ui.htmlAttribute (Html.Attributes.style "overflow" "hidden")
                    , paddingXY 20 40
                    , background (rgba 0 0 0 0.15)
                    , inFront <|
                        el
                            [ alignRight
                            , alignTop
                            , padding 16
                            , Font.size 28
                            , pointer
                            ]
                            (text "✕")
                    ]

                else
                    []
               )
        )
        [ inputView attrs filteredOptions config viewConfig
        , if ViewConfig.shouldShowNoMatchElement filteredOptions select viewConfig then
            Maybe.withDefault defaultNoMatchElement viewConfig.noMatchElement

          else
            none
        , menuView
            (defaultMenuAttrs Placement.Below
                { menuWidth = Nothing
                , maxWidth = Nothing
                , menuHeight = Nothing
                }
                ++ (Ui.htmlAttribute (Html.Attributes.style "flex" "0 1 auto")
                        :: List.concatMap (\toAttrs -> toAttrs Placement.Below) viewConfig.menuAttributes
                   )
            )
            { menuId = Model.toMenuElementId select
            , toOptionId = Model.toOptionElementId select
            , toOptionState = Model.toOptionState select
            , onChange = config.onChange
            , menuOpen = Model.isOpen select
            , options = filteredOptions
            , optionElement = Maybe.withDefault (defaultOptionElement config.itemToString) viewConfig.optionElement
            , closeOnSelect = viewConfig.closeOnSelect
            }
        ]


inputView : List (Attribute msg) -> List (InternalOption.Option a) -> Config a msg -> ViewConfig a msg -> Element msg
inputView attrs filteredOptions ({ select } as config) viewConfig =
    Input.text
        -- Containers now width fill by default (instead of width shrink). I couldn't update that here so I recommend you review these attributes
        (List.concat
            [ attrs
            , [ ViewEvents.onFocus config.onChange config.itemToString select viewConfig filteredOptions
                    |> Ui.htmlAttribute
              , Events.onClick (InputClicked |> config.onChange)
              , Events.onLoseFocus
                    (config.onChange
                        (InputLostFocus
                            { clearInputValue = viewConfig.clearInputValueOnBlur
                            , selectExactMatch = viewConfig.selectExactMatchOnBlur
                            }
                            filteredOptions
                        )
                    )
              , Ui.htmlAttribute <|
                    ViewEvents.onKeyDown (Model.isOpen select)
                        (KeyDown
                            { selectOnTab = viewConfig.selectOnTab
                            , closeOnSelect = viewConfig.closeOnSelect
                            }
                            filteredOptions
                            >> config.onChange
                        )
              , Ui.htmlAttribute (Html.Attributes.id <| Model.toInputElementId select)
              , inFront <|
                    if Model.toValue select /= Nothing || Model.toInputValue select /= "" then
                        viewConfig.clearButton
                            |> Maybe.map (\( attrs_, el ) -> clearButtonElement config.onChange attrs_ el)
                            |> Maybe.withDefault none

                    else
                        none
              ]
            , List.map Ui.htmlAttribute (View.inputAccessibilityAttributes select)
            , [ below <|
                    if Model.isOpen select then
                        html <| View.ariaLive (List.length filteredOptions)

                    else
                        none
              ]
            ]
        )
        { onChange = ViewEvents.onInput config.onChange config.itemToString select viewConfig
        , text = Model.toInputText config.itemToString select
        , placeholder = config.placeholder
        , label = config.label
        }


menuView :
    List (Attribute msg)
    ->
        { menuId : String
        , toOptionId : Int -> String
        , toOptionState : ( Int, a ) -> OptionState
        , onChange : Msg a -> msg
        , menuOpen : Bool
        , options : List (InternalOption.Option a)
        , optionElement : OptionState -> a -> Element msg
        , closeOnSelect : Bool
        }
    -> Element msg
menuView attribs v =
    List.indexedMap (optionElement v) v.options
        |> column
            (attribs
                ++ (Ui.htmlAttribute <| Html.Attributes.id v.menuId)
                :: (if v.menuOpen && List.length v.options > 0 then
                        []

                    else
                        [ style "visibility" "hidden"
                        , htmlAttribute "aria-visible"
                            (if v.menuOpen then
                                "false"

                             else
                                "true"
                            )
                        , height (px 0)
                        , clipY
                        ]
                   )
            )


optionElement :
    { b
        | toOptionState : ( Int, a ) -> OptionState
        , toOptionId : Int -> String
        , onChange : Msg a -> msg
        , optionElement : OptionState -> a -> Element msg
        , closeOnSelect : Bool
    }
    -> Int
    -> InternalOption.Option a
    -> Element msg
optionElement v i opt =
    let
        optionState =
            v.toOptionState ( i, InternalOption.toItem opt )
    in
    row
        -- Containers now width fill by default (instead of width shrink). I couldn't update that here so I recommend you review these attributes
        ([ Ui.htmlAttribute (Html.Attributes.id (v.toOptionId i))
         , htmlAttribute "role" "option"
         , htmlAttribute "value" (InternalOption.toString opt)
         , Ui.htmlAttribute (Html.Events.preventDefaultOn "mousedown" (Decode.succeed ( v.onChange NoOp, True )))
         , Ui.htmlAttribute (Html.Events.preventDefaultOn "click" (Decode.succeed ( v.onChange <| OptionClicked v.closeOnSelect opt, True )))
         , width fill
         ]
            ++ (if optionState /= Highlighted then
                    [ Events.onMouseMove (v.onChange <| MouseEnteredOption i) ]

                else
                    []
               )
        )
        [ v.optionElement optionState (InternalOption.toItem opt) ]


clearButtonElement : (Msg a -> msg) -> List (Attribute msg) -> Element msg -> Element msg
clearButtonElement onChange attribs element =
    el
        (attribs
            ++ [ Ui.htmlAttribute <|
                    Html.Events.preventDefaultOn "click" (Decode.succeed ( onChange ClearButtonPressed, True ))
               , Region.description "clear"
               , Input.button (onChange ClearButtonPressed)
               ]
        )
        element


attributeNone : Attribute msg
attributeNone =
    Ui.htmlAttribute <| Html.Attributes.class ""


defaultMenuAttrs :
    Placement
    ->
        { menuWidth : Maybe Int
        , maxWidth : Maybe Int
        , menuHeight : Maybe Int
        }
    -> List (Attribute msg)
defaultMenuAttrs placement { menuWidth, maxWidth, menuHeight } =
    (case menuWidth of
        Just w ->
            [ width shrink, widthMin w, widthMax (Maybe.withDefault w maxWidth) ]

        Nothing ->
            []
    )
        ++ [ case menuHeight of
                Just menuHeight_ ->
                    heightMax menuHeight_

                Nothing ->
                    attributeNone
           , height shrink

           --   , style "overflow-y" "auto"
           , borderColor (rgb 204 204 204)
           , border 1
           , rounded 5
           , background (rgb 255 255 255)
           , paddingXY 0 5
           , htmlAttribute "role" "listbox"
           , move <|
                case placement of
                    Placement.Above ->
                        up 10

                    Placement.Below ->
                        down 0
           ]


positionFixedEl : Placement -> Maybe Dom.Element -> Element msg -> Element msg
positionFixedEl placement container =
    el
        -- Containers now width fill by default (instead of width shrink). I couldn't update that here so I recommend you review these attributes
        (style "position" "fixed"
            :: (if placement == Placement.Above then
                    [ style "transform"
                        ("translateY(calc(-100% - 5px - "
                            ++ (Maybe.map (.element >> .height >> String.fromFloat) container |> Maybe.withDefault "0")
                            ++ "px))"
                        )
                    ]

                else
                    []
               )
        )


defaultOptionElement : (a -> String) -> OptionState -> a -> Element msg
defaultOptionElement toString optionState a =
    el
        [ pointer
        , paddingXY 14 10
        , background <|
            case optionState of
                Highlighted ->
                    rgb 227 227 227

                Selected ->
                    rgba 164 211 249 0.8

                SelectedAndHighlighted ->
                    rgba 164 211 249 1

                Idle ->
                    rgb 255 255 255
        ]
        (Prose.paragraph [ width shrink ] [ text (toString a) ])


defaultNoMatchElement : Element msg
defaultNoMatchElement =
    el
        [ padding 5
        , borderColor (rgb 204 204 204)
        , border 1
        , rounded 5
        , background (rgb 255 255 255)
        ]
        (text "No matches")


htmlAttribute : String -> String -> Attribute msg
htmlAttribute prop val =
    Ui.htmlAttribute (Html.Attributes.attribute prop val)


style : String -> String -> Attribute msg
style prop val =
    Ui.htmlAttribute (Html.Attributes.style prop val)
