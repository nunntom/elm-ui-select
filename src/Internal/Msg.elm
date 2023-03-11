module Internal.Msg exposing (Msg(..))

import Browser.Dom as Dom
import Internal.Option exposing (Option)


type Msg a
    = InputChanged String (List (Option a))
    | OptionClicked (Option a)
    | InputFocused (Maybe Int)
    | GotNewFilteredOptions (List (Option a))
    | InputClicked (Maybe Int)
    | InputLostFocus
        { clearInputValue : Bool
        , selectExactMatch : Bool
        }
        (List (Option a))
    | MouseEnteredOption Int
    | KeyDown Bool (List (Option a)) String
    | GotContainerAndMenuElements (Maybe Int) (Result Dom.Error { menu : Dom.Viewport, container : Dom.Element })
    | ClearButtonPressed
    | InputDebounceReturned String
    | GotRequestResponse String (Result String (List a))
    | NoOp
