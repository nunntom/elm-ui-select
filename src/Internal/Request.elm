module Internal.Request exposing
    ( Request
    , request
    , toDelay
    , toEffect
    , toMinLength
    , withDelay
    , withMinLength
    )


type Request effect
    = Request
        { effect : String -> effect
        , delay : Float
        , minLength : Int
        }


request : (String -> effect) -> Request effect
request effect =
    Request
        { effect = effect
        , delay = 300
        , minLength = 3
        }


withDelay : Float -> Request effect -> Request effect
withDelay delay (Request req) =
    Request { req | delay = delay }


withMinLength : Int -> Request effect -> Request effect
withMinLength len (Request req) =
    Request { req | minLength = len }


toEffect : Request effect -> (String -> effect)
toEffect (Request { effect }) =
    effect


toDelay : Request effect -> Float
toDelay (Request { delay }) =
    delay


toMinLength : Request effect -> Int
toMinLength (Request { minLength }) =
    minLength
