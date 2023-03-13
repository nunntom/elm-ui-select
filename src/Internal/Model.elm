module Internal.Model exposing
    ( Model
    , blur
    , clear
    , closeMenu
    , highlightIndex
    , init
    , isFocused
    , isLoading
    , isOpen
    , isRequestFailed
    , openMenu
    , requiresNewFilteredOptions
    , selectOption
    , setElements
    , setFilteredOptions
    , setFocused
    , setInputValue
    , setItems
    , setRequestState
    , setSelected
    , toContainerElement
    , toContainerElementId
    , toCurrentFilteredOptions
    , toFilteredOptions
    , toHighlighted
    , toInputElementId
    , toInputValue
    , toItems
    , toMenuElementId
    , toMenuMaxHeight
    , toMenuMinWidth
    , toMenuPlacement
    , toOptionElementId
    , toOptionState
    , toRequestState
    , toValue
    , wasHighlightedByMouse
    )

import Browser.Dom as Dom
import Internal.Filter as Filter exposing (Filter)
import Internal.Option as Option exposing (Option)
import Internal.OptionState exposing (OptionState(..))
import Internal.Placement exposing (Placement(..))
import Internal.RequestState exposing (RequestState(..))



-- MODEL


type Model a
    = Model (InternalState a)


type alias InternalState a =
    { id : String
    , items : List a
    , filteredOptions : Maybe (List (Option a))
    , selected : Maybe a
    , inputValue : String
    , highlighted : Maybe Int
    , highlightedByMouse : Bool
    , menuOpen : Bool
    , containerElement : Maybe Dom.Element
    , menuViewPort : Maybe Dom.Viewport
    , requestState : Maybe RequestState
    , applyFilter : Bool
    , focused : Bool
    }



-- INIT


init : String -> Model a
init id =
    Model
        { id = id
        , items = []
        , filteredOptions = Nothing
        , selected = Nothing
        , inputValue = ""
        , highlighted = Nothing
        , highlightedByMouse = False
        , menuOpen = False
        , containerElement = Nothing
        , menuViewPort = Nothing
        , requestState = Nothing
        , applyFilter = False
        , focused = False
        }



-- GET


toValue : Model a -> Maybe a
toValue (Model { selected }) =
    selected


toInputValue : Model a -> String
toInputValue (Model { inputValue }) =
    inputValue


toItems : Model a -> List a
toItems (Model { items }) =
    items


toRequestState : Model a -> Maybe RequestState
toRequestState (Model { requestState }) =
    requestState


toHighlighted : Model a -> Maybe Int
toHighlighted (Model { highlighted }) =
    highlighted


wasHighlightedByMouse : Model a -> Bool
wasHighlightedByMouse (Model { highlightedByMouse }) =
    highlightedByMouse


toFilteredOptions : Bool -> Maybe Int -> (a -> String) -> Maybe (Filter a) -> Model a -> List (Option a)
toFilteredOptions onlyIfFocused minInputLength itemToString filter (Model model) =
    if onlyIfFocused && not model.focused then
        []

    else
        case minInputLength of
            Just chars ->
                if String.length model.inputValue >= chars then
                    toFilteredOptions_ itemToString filter (Model model)

                else
                    []

            Nothing ->
                if model.applyFilter then
                    toFilteredOptions_ itemToString filter (Model model)

                else
                    List.map (Option.init itemToString) model.items


toFilteredOptions_ : (a -> String) -> Maybe (Filter a) -> Model a -> List (Option a)
toFilteredOptions_ itemToString filter (Model model) =
    case model.filteredOptions of
        Just opts ->
            opts

        Nothing ->
            List.map (Option.init itemToString) model.items
                |> Filter.filterOptions model.inputValue filter


toCurrentFilteredOptions : Model a -> List (Option a)
toCurrentFilteredOptions (Model { filteredOptions }) =
    Maybe.withDefault [] filteredOptions


toInputElementId : Model a -> String
toInputElementId (Model { id }) =
    id ++ "-input"


toContainerElementId : Model a -> String
toContainerElementId (Model { id }) =
    id ++ "-container"


toMenuElementId : Model a -> String
toMenuElementId (Model { id }) =
    id ++ "-menu"


toOptionElementId : Model a -> Int -> String
toOptionElementId (Model { id }) idx =
    id ++ "-option-" ++ String.fromInt idx


toOptionState : Model a -> ( Int, a ) -> OptionState
toOptionState (Model { highlighted, selected }) ( idx, a ) =
    if highlighted == Just idx && selected == Just a then
        SelectedAndHighlighted

    else if highlighted == Just idx then
        Highlighted

    else if selected == Just a then
        Selected

    else
        Idle


toMenuPlacement : Maybe Int -> Maybe Placement -> Model a -> Placement
toMenuPlacement maxHeight forcedPlacement (Model model) =
    Maybe.map2 (calculateMenuDimensionsAndPlacement maxHeight forcedPlacement) model.containerElement model.menuViewPort
        |> Maybe.map .placement
        |> Maybe.withDefault Below


toMenuMinWidth : Model a -> Maybe Int
toMenuMinWidth (Model model) =
    Maybe.map2 (calculateMenuDimensionsAndPlacement Nothing Nothing) model.containerElement model.menuViewPort
        |> Maybe.map .minWidth


toMenuMaxHeight : Maybe Int -> Maybe Placement -> Model a -> Maybe Int
toMenuMaxHeight maxHeight forcedPlacement (Model model) =
    Maybe.map2 (calculateMenuDimensionsAndPlacement maxHeight forcedPlacement) model.containerElement model.menuViewPort
        |> Maybe.map .maxHeight


toContainerElement : Model a -> Maybe Dom.Element
toContainerElement (Model { containerElement }) =
    containerElement



-- CHECKS


isOpen : Model a -> Bool
isOpen (Model { menuOpen }) =
    menuOpen


isFocused : Model a -> Bool
isFocused (Model { focused }) =
    focused


isLoading : Model a -> Bool
isLoading (Model { requestState }) =
    requestState == Just Loading


isRequestFailed : Model a -> Bool
isRequestFailed (Model { requestState }) =
    requestState == Just Failed


requiresNewFilteredOptions : Model a -> Bool
requiresNewFilteredOptions (Model { filteredOptions }) =
    filteredOptions == Nothing



-- UPDATE


setItems : List a -> Model a -> Model a
setItems items (Model model) =
    Model
        { model
            | items = items
            , filteredOptions =
                if items == model.items then
                    model.filteredOptions

                else
                    Nothing
        }


setSelected : Maybe a -> Model a -> Model a
setSelected a (Model model) =
    Model { model | selected = a }


setInputValue : String -> Model a -> Model a
setInputValue v (Model model) =
    Model
        { model
            | inputValue = v
            , applyFilter = True
            , filteredOptions =
                if v == model.inputValue then
                    model.filteredOptions

                else
                    Nothing
        }


setFilteredOptions : List (Option a) -> Model a -> Model a
setFilteredOptions opts (Model model) =
    Model { model | filteredOptions = Just opts }


selectOption : Option a -> Model a -> Model a
selectOption opt (Model model) =
    Model
        { model
            | menuOpen = False
            , selected = Just (Option.toItem opt)
            , highlighted = Nothing
            , inputValue = Option.toString opt
            , applyFilter = False
        }


highlightIndex : Maybe Int -> Bool -> Model a -> Model a
highlightIndex idx byMouse (Model model) =
    Model
        { model
            | highlighted = idx
            , highlightedByMouse = byMouse
        }


setRequestState : Maybe RequestState -> Model a -> Model a
setRequestState requestState (Model model) =
    Model { model | requestState = requestState }


openMenu : Model a -> Model a
openMenu (Model model) =
    Model { model | menuOpen = True }


closeMenu : Model a -> Model a
closeMenu (Model model) =
    Model
        { model
            | menuOpen = False
            , highlighted = Nothing
        }


clear : Model a -> Model a
clear (Model model) =
    Model
        { model
            | inputValue = ""
            , selected = Nothing
            , highlighted = Nothing
            , applyFilter = False
            , menuOpen = False
        }


setElements : { container : Maybe Dom.Element, menu : Maybe Dom.Viewport } -> Model a -> Model a
setElements { container, menu } (Model model) =
    Model
        { model
            | menuViewPort = menu
            , containerElement = container
        }


setFocused : Bool -> Model a -> Model a
setFocused v (Model model) =
    Model { model | focused = v }


blur :
    { clearInputValue : Bool
    , selectExactMatch : Bool
    }
    -> Bool
    -> List (Option a)
    -> Model a
    -> Model a
blur { clearInputValue, selectExactMatch } hasRequest filteredOptions (Model model) =
    (if model.selected == Nothing then
        case ( selectExactMatch, Option.findByString filteredOptions model.inputValue ) of
            ( True, Just option ) ->
                selectOption option (Model model)

            _ ->
                if clearInputValue then
                    Model
                        { model
                            | inputValue = ""
                            , items =
                                if not hasRequest then
                                    model.items

                                else
                                    []
                        }

                else
                    Model model

     else
        Model model
    )
        |> setFocused False
        |> closeMenu



-- INTERNAL


calculateMenuDimensionsAndPlacement : Maybe Int -> Maybe Placement -> Dom.Element -> Dom.Viewport -> { minWidth : Int, maxHeight : Int, placement : Placement }
calculateMenuDimensionsAndPlacement maxHeight forcedPlacement container menu =
    calculateMenuDimensionsAndPlacementHelper
        forcedPlacement
        { menuHeight =
            Maybe.map (Basics.min (Basics.round menu.scene.height)) maxHeight
                |> Maybe.withDefault (Basics.round menu.scene.height)
        , containerWidth = Basics.round container.element.width
        }
        (calculateSpace container)


calculateMenuDimensionsAndPlacementHelper : Maybe Placement -> { menuHeight : Int, containerWidth : Int } -> { above : Float, below : Float } -> { minWidth : Int, maxHeight : Int, placement : Placement }
calculateMenuDimensionsAndPlacementHelper forcedPlacement { menuHeight, containerWidth } { above, below } =
    if forcedPlacement == Just Above || (Basics.round below < menuHeight && above > below && forcedPlacement /= Just Below) then
        { minWidth = containerWidth
        , maxHeight =
            Basics.min menuHeight (Basics.round (above - 20))
        , placement = Above
        }

    else
        { minWidth = containerWidth
        , maxHeight = Basics.min menuHeight (Basics.round (below - 20))
        , placement = Below
        }


calculateSpace : Dom.Element -> { above : Float, below : Float }
calculateSpace { viewport, element } =
    { above = element.y - viewport.y
    , below = (viewport.y + viewport.height) - (element.y + element.height)
    }
