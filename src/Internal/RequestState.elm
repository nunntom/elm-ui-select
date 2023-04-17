module Internal.RequestState exposing (RequestState(..))


type RequestState
    = NotRequested
    | Loading String
    | Success String
    | Failed
