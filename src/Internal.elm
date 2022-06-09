module Internal exposing (optionId)


optionId : Int -> String -> String
optionId i id =
    id ++ "-" ++ String.fromInt i
