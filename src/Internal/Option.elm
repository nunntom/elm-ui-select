module Internal.Option exposing (Option, findByString, findByValue)


type alias Option a =
    ( a, String )


findByValue : List (Option a) -> a -> Maybe (Option a)
findByValue list a =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if a == Tuple.first x then
                Just x

            else
                findByValue xs a


findByString : List (Option a) -> String -> Maybe (Option a)
findByString list s =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if String.toLower s == String.toLower (Tuple.second x) then
                Just x

            else
                findByString xs s
