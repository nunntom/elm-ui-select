module Internal.Msg exposing (Msg(..))

import Browser.Dom as Dom
import Internal.Option exposing (Option)


type Msg a
    = InputChanged String (List (Option a))
    | OptionClicked Bool (Option a)
    | InputFocused
        { openMenu : Bool
        , isMobile : Bool
        }
        String
        (Maybe ( List a, List (Option a) ))
    | DelayAfterFocusFinished
    | GotNewFilteredOptions Bool ( List a, List (Option a) )
    | GotIsMobile Bool
    | InputClicked
    | InputLostFocus
        { clearInputValue : Bool
        , selectExactMatch : Bool
        }
        (List (Option a))
    | MouseEnteredOption Int
    | KeyDown
        { closeOnSelect : Bool
        , selectOnTab : Bool
        }
        (List (Option a))
        String
    | GotContainerAndMenuElements (Maybe Int) (Result Dom.Error { menu : Dom.Viewport, container : Dom.Element })
    | ClearButtonPressed
    | InputDebounceReturned String
    | GotRequestResponse String (Result String ( List a, Maybe a ))
    | MobileCloseButtonPressed
    | NoOp
