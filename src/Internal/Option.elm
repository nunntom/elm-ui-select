module Internal.Option exposing (Option, findByString, findByValue, findIndex, init, toItem, toString)

import List.Extra


type alias Option a =
    ( a, String )


init : (a -> String) -> a -> Option a
init itemToString item =
    ( item, itemToString item )


toItem : Option a -> a
toItem ( a, _ ) =
    a


toString : Option a -> String
toString ( _, s ) =
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


findIndex : List (Option a) -> a -> Maybe Int
findIndex list a =
    let
        findIndex_ idx l =
            case l of
                [] ->
                    Nothing

                x :: xs ->
                    if a == toItem x then
                        Just idx

                    else
                        findIndex_ (idx + 1) xs
    in
    findIndex_ 0 list


findByString : List (Option a) -> String -> Maybe (Option a)
findByString list s =
    List.Extra.find (\o -> String.toLower s == String.toLower (toString o)) list
