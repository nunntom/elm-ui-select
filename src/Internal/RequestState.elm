module Internal.RequestState exposing (RequestState(..))


type RequestState
    = NotRequested
    | Loading
    | Success
    | Failed
