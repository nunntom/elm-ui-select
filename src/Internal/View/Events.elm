module Internal.View.Events exposing (onFocus, onInput, onKeyDown, onStartDecoder, updateFilteredOptions)

import Html exposing (Attribute)
import Html.Events
import Internal.Model as Model exposing (Model)
import Internal.Msg exposing (Msg(..))
import Internal.Option exposing (Option)
import Internal.ViewConfig exposing (ViewConfigInternal)
import Json.Decode as Decode exposing (Decoder)


onKeyDown : Bool -> (String -> msg) -> Attribute msg
onKeyDown menuOpen tagger =
    Html.Events.custom "keydown"
        (Decode.map (hijackKey menuOpen tagger)
            (Decode.field "key" Decode.string)
        )


hijackKey :
    Bool
    -> (String -> msg)
    -> String
    ->
        { message : msg
        , stopPropagation : Bool
        , preventDefault : Bool
        }
hijackKey menuOpen tagger key =
    { message = tagger key
    , stopPropagation = menuOpen && key == "Escape"
    , preventDefault = List.member key [ "ArrowUp", "ArrowDown", "PageUp", "PageDown", "Enter" ]
    }


onInput : (Msg a -> msg) -> (a -> String) -> Model a -> ViewConfigInternal a attribute view -> String -> msg
onInput onChange itemToString model viewConfig v =
    InputChanged v
        (Model.onInputChange v model
            |> Model.toFilteredOptions False viewConfig.minInputLength itemToString viewConfig.filter
        )
        |> onChange


onFocus : (Msg a -> msg) -> (a -> String) -> Model a -> ViewConfigInternal a attribute view -> List (Option a) -> Attribute msg
onFocus onChange itemToString model viewConfig filteredOptions =
    let
        decodeEvent updateFiltered =
            Decode.map
                (\isMobile ->
                    onChange <|
                        InputFocused
                            { openMenu = viewConfig.openOnFocus
                            , isMobile = isMobile
                            }
                            (Model.toInputText itemToString model)
                            updateFiltered
                )
                (isMobileDecoder viewConfig)
    in
    Html.Events.on "focus" <|
        if Model.requiresNewFilteredOptions model then
            Decode.lazy (\_ -> decodeEvent (Just <| optionsUpdate itemToString model viewConfig filteredOptions))

        else
            decodeEvent Nothing


optionsUpdate : (a -> String) -> Model a -> ViewConfigInternal a attribute view -> List (Option a) -> ( List a, List (Option a) )
optionsUpdate itemToString model viewConfig filteredOptions =
    ( Model.toItems model
    , if List.isEmpty filteredOptions then
        Model.toFilteredOptions False viewConfig.minInputLength itemToString viewConfig.filter model

      else
        filteredOptions
    )


updateFilteredOptions : (Msg a -> msg) -> (a -> String) -> Model a -> ViewConfigInternal a attribute view -> List (Option a) -> List (Attribute msg)
updateFilteredOptions onChange itemToString model viewConfig filteredOptions =
    if Model.requiresNewFilteredOptions model then
        let
            decoder =
                newFilteredOptionsDecoder itemToString model viewConfig filteredOptions
                    |> Decode.map onChange
        in
        [ Html.Events.on "keydown" decoder
        , Html.Events.on "touchstart" decoder
        , Html.Events.on "mousemove" decoder
        ]

    else
        []


isMobileDecoder : ViewConfigInternal a attribute view -> Decoder Bool
isMobileDecoder viewConfig =
    Decode.maybe
        (Decode.oneOf
            [ Decode.at [ "view", "innerWidth" ] Decode.float
            , Decode.at [ "target", "ownerDocument", "defaultView", "innerWidth" ] Decode.float
            ]
        )
        |> Decode.map
            (\width ->
                Maybe.map2 (\w bp -> w <= bp) width viewConfig.mobileBreakpoint
                    |> Maybe.withDefault False
            )


newFilteredOptionsDecoder : (a -> String) -> Model a -> ViewConfigInternal a attribute view -> List (Option a) -> Decoder (Msg a)
newFilteredOptionsDecoder itemToString model viewConfig filteredOptions =
    let
        options _ =
            optionsUpdate itemToString model viewConfig filteredOptions
    in
    Decode.lazy (\_ -> Decode.map (\isMobile -> GotNewFilteredOptions isMobile (options ())) (isMobileDecoder viewConfig))


onStartDecoder : (Msg a -> msg) -> (a -> String) -> Model a -> ViewConfigInternal a attribute view -> List (Option a) -> Decoder msg
onStartDecoder onChange itemToString model viewConfig filteredOptions =
    if Model.requiresNewFilteredOptions model then
        newFilteredOptionsDecoder itemToString model viewConfig filteredOptions
            |> Decode.map onChange

    else
        isMobileDecoder viewConfig
            |> Decode.map (GotIsMobile >> onChange)
