module Internal.UpdateOptions exposing (UpdateOption(..), UpdateOptions, fromList)


type UpdateOption err effect a msg
    = Request (String -> (Result err (List a) -> msg) -> effect)
    | DebounceRequest Float
    | RequestMinInputLength Int
    | OnSelect (Maybe a -> msg)
    | OnFocus msg
    | OnLoseFocus msg
    | OnInput (String -> msg)
    | OnKeyDown (String -> msg)


type alias UpdateOptions err effect a msg =
    { request : Maybe (String -> (Result err (List a) -> msg) -> effect)
    , debounceRequest : Float
    , requestMinInputLength : Int
    , onSelect : Maybe (Maybe a -> msg)
    , onFocus : Maybe msg
    , onLoseFocus : Maybe msg
    , onInput : Maybe (String -> msg)
    , onKeyDown : Maybe (String -> msg)
    }


init : UpdateOptions err effect a msg
init =
    { request = Nothing
    , debounceRequest = 300
    , requestMinInputLength = 3
    , onSelect = Nothing
    , onFocus = Nothing
    , onLoseFocus = Nothing
    , onInput = Nothing
    , onKeyDown = Nothing
    }


fromList : List (UpdateOption err effect a msg) -> UpdateOptions err effect a msg
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

                OnKeyDown msg ->
                    { opts | onKeyDown = Just msg }
        )
        init
