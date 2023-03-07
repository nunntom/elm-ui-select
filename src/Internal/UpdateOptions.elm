module Internal.UpdateOptions exposing (UpdateOption(..), UpdateOptions, fromList)


type UpdateOption effect a msg
    = Request (String -> (Result String (List a) -> msg) -> effect)
    | DebounceRequest Float
    | RequestMinInputLength Int
    | OnSelect (Maybe a -> msg)
    | OnFocus msg
    | OnLoseFocus msg
    | OnInput (String -> msg)


type alias UpdateOptions effect a msg =
    { request : Maybe (String -> (Result String (List a) -> msg) -> effect)
    , debounceRequest : Float
    , requestMinInputLength : Int
    , onSelect : Maybe (Maybe a -> msg)
    , onFocus : Maybe msg
    , onLoseFocus : Maybe msg
    , onInput : Maybe (String -> msg)
    }


init : UpdateOptions effect a msg
init =
    { request = Nothing
    , debounceRequest = 300
    , requestMinInputLength = 3
    , onSelect = Nothing
    , onFocus = Nothing
    , onLoseFocus = Nothing
    , onInput = Nothing
    }


fromList : List (UpdateOption effect a msg) -> UpdateOptions effect a msg
fromList =
    List.foldl
        (\opt opts ->
            case opt of
                Request req ->
                    { opts | request = Just req }

                DebounceRequest delay ->
                    { opts | debounceRequest = delay }

                RequestMinInputLength len ->
                    { opts | requestMinInputLength = len }

                OnSelect msg ->
                    { opts | onSelect = Just msg }

                OnFocus msg ->
                    { opts | onFocus = Just msg }

                OnLoseFocus msg ->
                    { opts | onLoseFocus = Just msg }

                OnInput msg ->
                    { opts | onInput = Just msg }
        )
        init
