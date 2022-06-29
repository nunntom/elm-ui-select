module Select exposing
    ( Select, init
    , setItems, setSelected, setInputValue
    , toValue, toInputValue
    , isMenuOpen, isLoading, isRequestFailed
    , Msg, update, updateWithRequest, Request, request, gotRequestResponse
    , view, withMenuAttributes, withMenuMaxHeight, withMenuMaxWidth, withNoMatchElement, OptionState, withOptionElement, ClearButton, withClearButton, clearButton, withFilter, withMenuAlwaysAbove, withMenuAlwaysBelow
    , toElement
    , Effect
    )

{-| A select widget for elm-ui.


# Type

@docs Select, init


# Set

@docs setItems, setSelected, setInputValue


# Get

@docs toValue, toInputValue


# Check

@docs isMenuOpen, isLoading, isRequestFailed


# Update and Requests

@docs Msg, update, updateWithRequest, Request, request, gotRequestResponse


# Configure View

@docs view, withMenuAttributes, withMenuMaxHeight, withMenuMaxWidth, withNoMatchElement, OptionState, withOptionElement, ClearButton, withClearButton, clearButton, withFilter, withMenuAlwaysAbove, withMenuAlwaysBelow


# Element

@docs toElement


# Effect

@docs Effect

-}

import Element exposing (Attribute, Element)
import Element.Input as Input
import Internal.Effect as Effect
import Internal.Model as Model exposing (Model)
import Internal.Msg as Msg
import Internal.OptionState as OptionState
import Internal.Placement exposing (Placement(..))
import Internal.Request as Request
import Internal.Update as Update
import Internal.View as View exposing (ViewConfigInternal)
import Select.Filter exposing (Filter)



-- MODEL


{-| The main Select type
-}
type alias Select a =
    Model a


{-| Initialise the Select. You must provide a unique id. The id will be used for getting DOM elements etc.
-}
init : String -> Select a
init =
    Model.init


{-| Set the list of items
-}
setItems : List a -> Select a -> Select a
setItems =
    Model.setItems


{-| Set the selected item
-}
setSelected : Maybe a -> Select a -> Select a
setSelected =
    Model.setSelected


{-| Set the input value
-}
setInputValue : String -> Select a -> Select a
setInputValue =
    Model.setInputValue



-- GET


{-| Get the selected item
-}
toValue : Select a -> Maybe a
toValue =
    Model.toValue


{-| Get the value of the input
-}
toInputValue : Select a -> String
toInputValue =
    Model.toInputValue



-- CHECK


{-| Is the menu open?
-}
isMenuOpen : Select a -> Bool
isMenuOpen =
    Model.isOpen


{-| Is there a request currently loading? Could be used to add loading styling.
-}
isLoading : Select a -> Bool
isLoading =
    Model.isLoading


{-| Did a request fail?
-}
isRequestFailed : Select a -> Bool
isRequestFailed =
    Model.isRequestFailed



-- UPDATE


{-| The Msg type
-}
type alias Msg a =
    Msg.Msg a


{-| Update the Select
-}
update : (Msg a -> msg) -> Msg a -> Select a -> ( Select a, Cmd msg )
update tagger msg select =
    Update.update tagger Nothing msg select
        |> Tuple.mapSecond (Effect.perform (\_ -> Cmd.none))


{-| Update with an HTTP request to retrieve matching remote results.
Note that in order to avoid an elm/http dependency in this package, you will need to provide the request Cmd yourself.
-}
updateWithRequest : (Msg a -> msg) -> Request a -> Msg a -> Select a -> ( Select a, Cmd msg )
updateWithRequest tagger requestCmd msg select =
    Update.update identity (Just (Request.request identity)) msg select
        |> Tuple.mapSecond (Effect.perform (Request.toEffect requestCmd) >> Cmd.map tagger)


{-| A Request. See [Select.Request](Select-Request) for configuration options.
-}
type alias Request a =
    Request.Request (Cmd (Msg a))


{-| Create a request. Provide a function that takes the input value and returns a Cmd.
Update will return this Cmd when the user types in the input subject to a debounce delay
and minimum number of characters which can be configured in the [Select.Request](Select-Request) module.
-}
request : (String -> Cmd (Msg a)) -> Request a
request =
    Request.request


{-| Hook the request Cmd result back into update with this Msg
-}
gotRequestResponse : Result err (List a) -> Msg a
gotRequestResponse =
    Result.mapError (\_ -> ()) >> Msg.GotRequestResponse



-- VIEW


{-| The View Configuration
-}
type ViewConfig a msg
    = ViewConfig (ViewConfigInternal a msg)


{-| Initialise the default ViewConfig
-}
view :
    List (Attribute msg)
    ->
        { onChange : Msg a -> msg
        , itemToString : a -> String
        , label : Input.Label msg
        , placeholder : Maybe (Input.Placeholder msg)
        }
    -> ViewConfig a msg
view attribs config =
    ViewConfig (View.view attribs config)


{-| Customise the filtering of the menu based on input value. See [Select.Filter](Select-Filter). Default is startsWithThenContains.
-}
withFilter : Maybe (Filter a) -> ViewConfig a msg -> ViewConfig a msg
withFilter filter (ViewConfig config) =
    ViewConfig { config | filter = filter }


{-| Force the menu to always appear below the input. You may use this for examply if you have issues with an input inside a scrollable transformed container.
By default the menu will try to detect whether there is more space above or below and appear there, preferring below.
-}
withMenuAlwaysBelow : ViewConfig a msg -> ViewConfig a msg
withMenuAlwaysBelow (ViewConfig config) =
    ViewConfig { config | menuPlacement = Just Below }


{-| Force the menu to always appear above the input. You may use this for examply if you have issues with an input inside a scrollable transformed container.
-}
withMenuAlwaysAbove : ViewConfig a msg -> ViewConfig a msg
withMenuAlwaysAbove (ViewConfig config) =
    ViewConfig { config | menuPlacement = Just Above }


{-| Set a maximum height for the menu
-}
withMenuMaxHeight : Maybe Int -> ViewConfig a msg -> ViewConfig a msg
withMenuMaxHeight height (ViewConfig config) =
    ViewConfig { config | menuMaxHeight = height }


{-| Set a maximum width for the menu
-}
withMenuMaxWidth : Maybe Int -> ViewConfig a msg -> ViewConfig a msg
withMenuMaxWidth width (ViewConfig config) =
    ViewConfig { config | menuMaxWidth = width }


{-| Set arbitrary attributes for the menu dropdown element. You can call this multiple times and it will accumulate attributes.
-}
withMenuAttributes : List (Attribute msg) -> ViewConfig a msg -> ViewConfig a msg
withMenuAttributes attribs (ViewConfig config) =
    ViewConfig { config | menuAttributes = config.menuAttributes ++ attribs }


{-| Provide your own element for the options in the menu, based on the current [state](#OptionState) of the option.
-}
withOptionElement : (OptionState -> a -> Element msg) -> ViewConfig a msg -> ViewConfig a msg
withOptionElement toEl (ViewConfig config) =
    ViewConfig { config | optionElement = \state -> toEl (mapOptionState state) }


{-| Option state for use with custom option element
-}
type OptionState
    = Idle
    | Highlighted
    | Selected


{-| Provide your own element to show when there are no matches based on the filter and input value. This appears below the input.
-}
withNoMatchElement : Element msg -> ViewConfig a msg -> ViewConfig a msg
withNoMatchElement element (ViewConfig config) =
    ViewConfig { config | noMatchElement = element }


{-| Add a button to clear the input. This element is positioned as Element.inFront.
-}
withClearButton : Maybe (ClearButton msg) -> ViewConfig a msg -> ViewConfig a msg
withClearButton cb (ViewConfig config) =
    ViewConfig { config | clearButton = Maybe.map (\(ClearButton attribs label) -> View.clearButtonElement config.onChange attribs label) cb }


{-| A button to clear the input
-}
type ClearButton msg
    = ClearButton (List (Attribute msg)) (Element msg)


{-| Create a clear button
-}
clearButton : List (Attribute msg) -> Element msg -> ClearButton msg
clearButton attribs label =
    ClearButton attribs label


{-| Turn the ViewConfig into an Element.
-}
toElement : Select a -> ViewConfig a msg -> Element msg
toElement model (ViewConfig config) =
    View.toElement model config



-- EFFECT


{-| For use with the [Effect pattern](https://sporto.github.io/elm-patterns/architecture/effects.html)
-}
type alias Effect effect msg =
    Effect.Effect effect msg



-- INTERNAL


mapOptionState : OptionState.OptionState -> OptionState
mapOptionState state =
    case state of
        OptionState.Idle ->
            Idle

        OptionState.Highlighted ->
            Highlighted

        OptionState.Selected ->
            Selected
