module Internal.Option exposing (Option, findByString, findByValue, init, toItem, toString)


type alias Option a =
    ( a, String, Int )


init : (a -> String) -> Int -> a -> Option a
init itemToString idx item =
    ( item, itemToString item, idx )


toItem : Option a -> a
toItem ( a, _, _ ) =
    a


toString : Option a -> String
toString ( _, s, _ ) =
    s


findByValue : List (Option a) -> a -> Maybe (Option a)
findByValue list a =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if a == toItem x then
                Just x

            else
                findByValue xs a


findByString : List (Option a) -> String -> Maybe (Option a)
findByString list s =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if String.toLower s == String.toLower (toString x) then
                Just x

            else
                findByString xs s
