module Internal.UpdateOptions exposing (UpdateOption(..), UpdateOptions, fromList)


type UpdateOption effect a msg
    = Request (String -> effect)
    | DebounceRequest Float
    | RequestMinInputLength Int
    | OnSelect (Maybe a -> msg)


type alias UpdateOptions effect a msg =
    { request : Maybe (String -> effect)
    , debounceRequest : Float
    , requestMinInputLength : Int
    , onSelect : Maybe (Maybe a -> msg)
    }


init : UpdateOptions effect a msg
init =
    { request = Nothing
    , debounceRequest = 300
    , requestMinInputLength = 3
    , onSelect = Nothing
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
        )
        init
