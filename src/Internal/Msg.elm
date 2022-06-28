module Internal.Msg exposing (Msg(..))

import Browser.Dom as Dom exposing (Element)
import Internal.Option exposing (Option)


type Msg a
    = InputChanged String
    | OptionClicked (Option a)
    | InputFocused
    | InputClicked
    | InputLostFocus
    | MouseEnteredOption Int
    | KeyDown (List (Option a)) String
    | GotContainerAndMenuElements (Result Dom.Error { menu : Element, container : Element })
    | ClearButtonPressed
    | InputDebounceReturned String
    | GotRequestResponse (Result () (List a))
    | NoOp
