module Internal.View.Events exposing (onFocus, onInput, onKeyDown, updateFilteredOptions)

import Html exposing (Attribute)
import Html.Events
import Internal.Model as Model exposing (Model)
import Internal.Msg exposing (Msg(..))
import Internal.Option exposing (Option)
import Internal.ViewConfig exposing (ViewConfigInternal)
import Json.Decode as Decode


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
                (\width ->
                    onChange <|
                        InputFocused
                            { openMenu = viewConfig.openOnFocus
                            , isMobile =
                                Maybe.map2 (\w bp -> w <= bp) width viewConfig.mobileBreakpoint
                                    |> Maybe.withDefault False
                            }
                            (Model.toInputText itemToString model)
                            updateFiltered
                )
                (Decode.maybe <| Decode.at [ "view", "innerWidth" ] Decode.float)
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
            options _ =
                optionsUpdate itemToString model viewConfig filteredOptions
        in
        [ Html.Events.on "keydown" (Decode.lazy (\_ -> Decode.succeed (GotNewFilteredOptions (options ()) |> onChange)))
        , Html.Events.on "touchstart" (Decode.lazy (\_ -> Decode.succeed (GotNewFilteredOptions (options ()) |> onChange)))
        , Html.Events.on "mousemove" (Decode.lazy (\_ -> Decode.succeed (GotNewFilteredOptions (options ()) |> onChange)))
        ]

    else
        []
