module Select.Request exposing (withDelay, withMinLength)

{-| You don't need to use this module unless you want to customise the request debounce delay and minimum character count to perform a request.


# Type

@docs withDelay, withMinLength

-}

import Internal.Request as Request exposing (Request)


{-| For debouncing. How long should we wait after a user stops typing before sending a request?
-}
withDelay : Float -> Request effect -> Request effect
withDelay =
    Request.withDelay


{-| How many characters does a user need to type before a request is sent.
If this is too low you may get an unmanagable number of results!
-}
withMinLength : Int -> Request effect -> Request effect
withMinLength =
    Request.withMinLength
