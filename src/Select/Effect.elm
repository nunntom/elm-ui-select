module Select.Effect exposing
    ( Effect
    , update, updateWithRequest, Request, request
    , perform, performWithRequest
    )

{-| Update the Select using the [Effects pattern](https://sporto.github.io/elm-patterns/architecture/effects.html)


# Type

@docs Effect


# Update

@docs update, updateWithRequest, Request, request


# Perform

@docs perform, performWithRequest

-}

import Internal.Effect as Effect
import Internal.Model exposing (Model)
import Internal.Msg exposing (Msg)
import Internal.Request as Request
import Internal.Update as Update


{-| -}
type alias Effect effect msg =
    Effect.Effect effect msg



-- UPDATE


{-| Update the Select
-}
update : (Msg a -> msg) -> Msg a -> Select a -> ( Select a, Effect Never msg )
update tagger =
    Update.update tagger Nothing


{-| Update with an HTTP request. Note that in order to avoid an elm/http dependency in this package, you will need to provide the request Cmd yourself.
-}
updateWithRequest : (Msg a -> msg) -> Request effect -> Msg a -> Select a -> ( Select a, Effect effect msg )
updateWithRequest tagger req =
    Update.update tagger (Just req)


{-| -}
type alias Request effect =
    Request.Request effect


{-| Create a request. Provide a function that takes the input value and returns an Effect (your app's own Effect type)
that can be used to perform an HTTP request. Update will use this Effect when the user types in the input subject to a debounce delay
and minimum number of characters which can be configured in the Request module.
-}
request : (String -> effect) -> Request effect
request =
    Request.request



-- PERFORM


{-| Turn an Effect into a Cmd
-}
perform : Effect Never msg -> Cmd msg
perform =
    Effect.perform (\_ -> Cmd.none)


{-| Perform the Effect with a request. You need to provide your own perform function to perform the provided request effect.
-}
performWithRequest : (effect -> Cmd msg) -> Effect effect msg -> Cmd msg
performWithRequest =
    Effect.perform



-- INTERNAL


type alias Select a =
    Model a
