module Internal.Msg exposing (Msg(..))

import Browser.Dom as Dom
import Internal.Option exposing (Option)


type Msg a
    = InputChanged String (List (Option a))
    | OptionClicked (Option a)
    | InputFocused
        { openMenu : Bool
        , mobileBreakpoint : Maybe Float
        }
        String
        (Maybe ( List a, List (Option a) ))
    | GotNewFilteredOptions ( List a, List (Option a) )
    | InputClicked
    | InputLostFocus
        { clearInputValue : Bool
        , selectExactMatch : Bool
        }
        (List (Option a))
    | MouseEnteredOption Int
    | KeyDown Bool (List (Option a)) String
    | GotContainerAndMenuElements (Maybe Float) (Maybe Int) (Result Dom.Error { menu : Dom.Viewport, container : Dom.Element })
    | ClearButtonPressed
    | InputDebounceReturned String
    | GotRequestResponse String (Result String ( List a, Maybe a ))
    | NoOp
