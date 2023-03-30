module Internal.Option exposing (Option, findByString, findIndex, init, toItem, toString)

import Internal.List.Extra as List


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


findIndex : List (Option a) -> a -> Maybe Int
findIndex list a =
    List.findIndex (\o -> a == toItem o) list


findByString : List (Option a) -> String -> Maybe (Option a)
findByString list s =
    List.find (\o -> String.toLower s == String.toLower (toString o)) list
