module Internal.Msg exposing (Msg(..))

import Browser.Dom as Dom
import Internal.Option exposing (Option)
import Internal.Placement exposing (Placement)


type Msg a
    = InputChanged String
    | OptionClicked (Option a)
    | InputFocused
    | InputClicked
    | InputLostFocus
    | MouseEnteredOption Int
    | KeyDown (List (Option a)) String
    | GotMenuDimensionsAndPlacement (Result Dom.Error ( { width : Int, height : Int }, Placement ))
    | GotScrollMenuResult (Result Dom.Error ())
    | ClearButtonPressed
    | InputDebounceReturned String
    | GotRequestResponse (Result () (List a))
