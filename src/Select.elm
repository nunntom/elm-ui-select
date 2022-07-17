module Select exposing
    ( Select, init
    , setItems, setSelected, setInputValue, closeMenu
    , toValue, toInputValue, toInputElementId, toMenuElementId
    , isMenuOpen, isLoading, isRequestFailed
    , Msg, update, updateWithRequest, UpdateConfig, Request, request, gotRequestResponse
    , ViewConfig, view, withMenuAttributes, MenuPlacement(..), withMenuMaxHeight, withMenuMaxWidth, withNoMatchElement, withOptionElement, OptionState, withClearButton, ClearButton, clearButton, withFilter, withMenuAlwaysAbove, withMenuAlwaysBelow, withMenuPositionFixed, withClearInputValueOnBlur, withSelectExactMatchOnBlur
    , toElement
    , Effect
    )

{-| A select widget for elm-ui.


# Type

@docs Select, init


# Set

@docs setItems, setSelected, setInputValue, closeMenu


# Get

@docs toValue, toInputValue, toInputElementId, toMenuElementId


# Check

@docs isMenuOpen, isLoading, isRequestFailed


# Update and Requests

@docs Msg, update, updateWithRequest, UpdateConfig, Request, request, gotRequestResponse


# Configure View

@docs ViewConfig, view, withMenuAttributes, MenuPlacement, withMenuMaxHeight, withMenuMaxWidth, withNoMatchElement, withOptionElement, OptionState, withClearButton, ClearButton, clearButton, withFilter, withMenuAlwaysAbove, withMenuAlwaysBelow, withMenuPositionFixed, withClearInputValueOnBlur, withSelectExactMatchOnBlur


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
import Internal.Placement as Placement
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


{-| Close the menu
-}
closeMenu : Select a -> Select a
closeMenu =
    Model.closeMenu



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


{-| Get the id of the DOM input element. Useful in tests or to associate the provided label with the input
-}
toInputElementId : Select a -> String
toInputElementId =
    Model.toInputElementId


{-| Get the id of the DOM menu container. Useful for testing
-}
toMenuElementId : Select a -> String
toMenuElementId =
    Model.toMenuElementId



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

    update : Msg -> Model -> ( Model, Cmd Msg )
        update msg model =
            case msg of
                SelectMsg subMsg ->
                    Select.update SelectMsg subMsg model.select
                        |> Tuple.mapFirst (\select -> { model | select = select })

-}
update : (Msg a -> msg) -> Msg a -> Select a -> ( Select a, Cmd msg )
update tagger msg select =
    Update.update Nothing tagger msg select
        |> Tuple.mapSecond (Effect.perform (\_ -> Cmd.none))


{-| Update with an HTTP request to retrieve matching remote results.
Note that in order to avoid an elm/http dependency in this package, you will need to provide the request Cmd yourself.

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            Request SelectMsg subMsg ->
                Select.updateWithRequest (Select.request fetchThings) SelectMsg subMsg model.select
                    |> Tuple.mapFirst (\select -> { model | select = select })

    fetchThings : String -> Cmd (Select.Msg Thing)
    fetchThings query =
        Http.get
            { url = "https://awesome-thing.api/things?search=" ++ query
            , expect = Http.expectJson (Select.gotRequestResponse query) (Decode.list thingDecoder)
            }

-}
updateWithRequest : Request a -> (Msg a -> msg) -> Msg a -> Select a -> ( Select a, Cmd msg )
updateWithRequest req tagger msg select =
    Update.update (Just req) identity msg select
        |> Tuple.mapSecond (Effect.perform identity >> Cmd.map tagger)


{-| Configuration options for updateWith.

  - Should the input be cleared if on blur if nothing is selected?
  - Should an exact string match (case insensitive) be selected automatically on blur if nothing is already selected?

If you do not provide a config, both default to False.

-}
type alias UpdateConfig =
    { clearInputValueOnBlur : Bool
    , selectExactMatchOnBlur : Bool
    }


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


{-| Hook the request Cmd result back into update with this Msg. You need to pass in the string query (input value)
that was used for the request. This is used to match up the correct response to most recent request in case of race conditions.
-}
gotRequestResponse : String -> Result err (List a) -> Msg a
gotRequestResponse inputValue =
    Result.mapError (\_ -> ()) >> Msg.GotRequestResponse inputValue



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


{-| Force the menu to always appear below the input. You may use this for example if you have issues with an input inside a scrollable transformed container.
By default the menu will try to detect whether there is more space above or below and appear there, preferring below.
-}
withMenuAlwaysBelow : ViewConfig a msg -> ViewConfig a msg
withMenuAlwaysBelow (ViewConfig config) =
    ViewConfig { config | menuPlacement = Just Placement.Below }


{-| Force the menu to always appear above the input. You may use this for example if you have issues with an input inside a scrollable transformed container.
-}
withMenuAlwaysAbove : ViewConfig a msg -> ViewConfig a msg
withMenuAlwaysAbove (ViewConfig config) =
    ViewConfig { config | menuPlacement = Just Placement.Above }


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


{-| Set arbitrary attributes for the menu element. You can call this multiple times and it will accumulate attributes.
You can define different attributes based on whether the menu appears above or below the input.
-}
withMenuAttributes : (MenuPlacement -> List (Attribute msg)) -> ViewConfig a msg -> ViewConfig a msg
withMenuAttributes attribs (ViewConfig config) =
    ViewConfig { config | menuAttributes = config.menuAttributes ++ [ mapPlacement >> attribs ] }


{-| Will the menu appear above or below the input?
-}
type MenuPlacement
    = MenuAbove
    | MenuBelow


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
    | SelectedAndHighlighted


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


{-| Use style: position fixed for the menu. This can be used if the select is inside a scrollable container to allow the menu to overflow the parent.
Note that if any transforms (e.g. Element.moveUp/Element.moveLeft) are applied to the parent, this no longer works and the menu will be clipped.
This is due to [a feature of the current CSS spec](https://bugs.chromium.org/p/chromium/issues/detail?id=20574).
Also if the container or window is scrolled or resized without the input losing focus, the menu will appear detached from the input!
To overcome this you may want to listen to scroll and resize events on the parent and window and use [closeMenu](#closeMenu) to hide the menu.
-}
withMenuPositionFixed : Bool -> ViewConfig a msg -> ViewConfig a msg
withMenuPositionFixed v (ViewConfig config) =
    ViewConfig { config | positionFixed = v }


{-| Should the input value be cleared when the input loses focus if nothing is selected?
-}
withClearInputValueOnBlur : Bool -> ViewConfig a msg -> ViewConfig a msg
withClearInputValueOnBlur v (ViewConfig config) =
    ViewConfig { config | clearInputValueOnBlur = v }


{-| If nothing is selected, but the input value matches exactly one of the options (case insensitive),
should we select it automatically when the input loses focus?
-}
withSelectExactMatchOnBlur : Bool -> ViewConfig a msg -> ViewConfig a msg
withSelectExactMatchOnBlur v (ViewConfig config) =
    ViewConfig { config | selectExactMatchOnBlur = v }


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

        OptionState.SelectedAndHighlighted ->
            SelectedAndHighlighted


mapPlacement : Placement.Placement -> MenuPlacement
mapPlacement placement =
    case placement of
        Placement.Above ->
            MenuAbove

        Placement.Below ->
            MenuBelow
