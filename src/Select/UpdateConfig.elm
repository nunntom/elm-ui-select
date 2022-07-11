module Select.UpdateConfig exposing
    ( UpdateConfig, default
    , withRequest, withClearInputValueOnBlur, withSelectExactMatchOnBlur
    )

{-| Build a config for updating the select


# Type

@docs UpdateConfig, default


# Configure

@docs withRequest, withClearInputValueOnBlur, withSelectExactMatchOnBlur

-}

import Internal.Request exposing (Request)


{-| The UpdateConfig type
-}
type alias UpdateConfig effect =
    { request : Maybe (Request effect)
    , clearInputValueOnBlur : Bool
    , selectExactMatchOnBlur : Bool
    }


{-| The default config. No request, clears the input on blur if no selection and does not select an exact string match on blur.
-}
default : UpdateConfig Never
default =
    { request = Nothing
    , clearInputValueOnBlur = False
    , selectExactMatchOnBlur = False
    }


{-| Update with an HTTP request to retrieve matching remote results.
Note that in order to avoid an elm/http dependency in this package, you will need to provide the request Cmd yourself.

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            Request SelectMsg subMsg ->
                Select.updateWith
                    (Select.UpdateConfig.default
                        |> Select.UpdateConfig.withRequest (Select.request fetchThings)
                    )
                    SelectMsg
                    subMsg
                    model.select
                    |> Tuple.mapFirst (\select -> { model | select = select })

    fetchThings : String -> Cmd (Select.Msg Thing)
    fetchThings query =
        Http.get
            { url = "https://awesome-thing.api/things?search=" ++ query
            , expect = Http.expectJson (Select.gotRequestResponse query) (Decode.list thingDecoder)
            }

-}
withRequest : Request effect -> UpdateConfig eff1 -> UpdateConfig effect
withRequest request config =
    { request = Just request
    , clearInputValueOnBlur = config.clearInputValueOnBlur
    , selectExactMatchOnBlur = config.selectExactMatchOnBlur
    }


{-| Should the input value be cleared when the input loses focus? Setting this to False can be useful to make the select more like an autocomplete
-}
withClearInputValueOnBlur : Bool -> UpdateConfig effect -> UpdateConfig effect
withClearInputValueOnBlur v config =
    { config | clearInputValueOnBlur = v }


{-| If the input value exactly matches the valueToString value of one of the options, set is as selected when the input loses focus.
-}
withSelectExactMatchOnBlur : Bool -> UpdateConfig effect -> UpdateConfig effect
withSelectExactMatchOnBlur v config =
    { config | selectExactMatchOnBlur = v }
