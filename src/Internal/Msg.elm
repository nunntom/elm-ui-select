module Internal.Msg exposing (Msg(..))

import Browser.Dom as Dom
import Internal.Option exposing (Option)


type Msg a
    = InputChanged String
    | OptionClicked (Option a)
    | InputFocused
    | InputClicked
    | InputLostFocus
        { clearInputValue : Bool
        , selectExactMatch : Bool
        }
        (List (Option a))
    | MouseEnteredOption Int
    | KeyDown Bool (List (Option a)) String
    | GotContainerAndMenuElements (Result Dom.Error { menu : Dom.Viewport, container : Dom.Element })
    | ClearButtonPressed
    | InputDebounceReturned String
    | GotRequestResponse String (Result () (List a))
    | NoOp
