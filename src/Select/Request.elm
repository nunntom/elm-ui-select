module Select.Request exposing (Request, request, toDelay, toEffect, toMinLength, withDelay, withMinLength)


type Request eff
    = Request
        { effect : String -> eff
        , delay : Float
        , minLength : Int
        }


request : (String -> eff) -> Request eff
request eff =
    Request
        { effect = eff
        , delay = 300
        , minLength = 3
        }


withDelay : Float -> Request eff -> Request eff
withDelay delay (Request req) =
    Request { req | delay = delay }


withMinLength : Int -> Request eff -> Request eff
withMinLength len (Request req) =
    Request { req | minLength = len }


toEffect : Request eff -> (String -> eff)
toEffect (Request { effect }) =
    effect


toDelay : Request eff -> Float
toDelay (Request { delay }) =
    delay


toMinLength : Request eff -> Int
toMinLength (Request { minLength }) =
    minLength
