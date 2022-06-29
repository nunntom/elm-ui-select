module Internal.Model exposing
    ( Model
    , applyFilter
    , clear
    , closeMenu
    , highlightIndex
    , init
    , isLoading
    , isOpen
    , isRequestFailed
    , openMenu
    , selectOption
    , setElements
    , setInputValue
    , setItems
    , setRequestState
    , setSelected
    , toContainerElementId
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
    )

import Browser.Dom as Dom
import Internal.Filter as Filter exposing (Filter)
import Internal.Option exposing (Option)
import Internal.OptionState exposing (OptionState(..))
import Internal.Placement exposing (Placement(..))
import Internal.RequestState exposing (RequestState(..))



-- MODEL


type Model a
    = Model (InternalState a)


type alias InternalState a =
    { id : String
    , items : List a
    , selected : Maybe a
    , inputValue : String
    , highlighted : Int
    , menuOpen : Bool
    , containerElement : Maybe Dom.Element
    , menuElement : Maybe Dom.Element
    , requestState : Maybe RequestState
    , applyFilter : Bool
    }



-- INIT


init : String -> Model a
init id =
    Model
        { id = id
        , items = []
        , selected = Nothing
        , inputValue = ""
        , highlighted = 0
        , menuOpen = False
        , containerElement = Nothing
        , menuElement = Nothing
        , requestState = Nothing
        , applyFilter = False
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


toHighlighted : Model a -> Int
toHighlighted (Model { highlighted }) =
    highlighted


toFilteredOptions : (a -> String) -> Maybe (Filter a) -> Model a -> List (Option a)
toFilteredOptions itemToString filter (Model model) =
    List.map (\i -> ( i, itemToString i )) model.items
        |> (if model.applyFilter then
                Filter.filterOptions model.inputValue filter

            else
                identity
           )


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
    if highlighted == idx then
        Highlighted

    else if selected == Just a then
        Selected

    else
        Idle



-- CHECKS


isOpen : Model a -> Bool
isOpen (Model { menuOpen }) =
    menuOpen


isLoading : Model a -> Bool
isLoading (Model { requestState }) =
    requestState == Just Loading


isRequestFailed : Model a -> Bool
isRequestFailed (Model { requestState }) =
    requestState == Just Failed


toMenuPlacement : Model a -> Placement
toMenuPlacement (Model model) =
    Maybe.map2 calculateMenuDimensionsAndPlacement model.containerElement model.menuElement
        |> Maybe.map .placement
        |> Maybe.withDefault Below


toMenuMinWidth : Model a -> Maybe Int
toMenuMinWidth (Model model) =
    Maybe.map2 calculateMenuDimensionsAndPlacement model.containerElement model.menuElement
        |> Maybe.map .minWidth


toMenuMaxHeight : Model a -> Maybe Int
toMenuMaxHeight (Model model) =
    Maybe.map2 calculateMenuDimensionsAndPlacement model.containerElement model.menuElement
        |> Maybe.map .maxHeight



-- UPDATE


setItems : List a -> Model a -> Model a
setItems items (Model d) =
    Model { d | items = items }


setSelected : Maybe a -> Model a -> Model a
setSelected a (Model d) =
    Model { d | selected = a }


setInputValue : String -> Model a -> Model a
setInputValue v (Model d) =
    Model { d | inputValue = v }


selectOption : Option a -> Model a -> Model a
selectOption ( a, s ) (Model model) =
    Model
        { model
            | menuOpen = False
            , selected = Just a
            , inputValue = s
            , applyFilter = False
        }


highlightIndex : Int -> Model a -> Model a
highlightIndex idx (Model model) =
    Model { model | highlighted = idx }


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
            , highlighted = 0
        }


clear : Model a -> Model a
clear (Model model) =
    Model
        { model
            | inputValue = ""
            , selected = Nothing
            , highlighted = 0
            , applyFilter = False
            , menuOpen = False
        }


setElements : { container : Maybe Dom.Element, menu : Maybe Dom.Element } -> Model a -> Model a
setElements { container, menu } (Model model) =
    Model
        { model
            | menuElement = menu
            , containerElement = container
        }


applyFilter : Bool -> Model a -> Model a
applyFilter v (Model model) =
    Model { model | applyFilter = v }



-- INTERNAL


calculateMenuDimensionsAndPlacement : Dom.Element -> Dom.Element -> { minWidth : Int, maxHeight : Int, placement : Placement }
calculateMenuDimensionsAndPlacement container menu =
    calculateMenuDimensionsAndPlacementHelper
        { menuSceneHeight = menu.scene.height
        , containerWidth = container.element.width
        }
        (calculateSpace container)


calculateMenuDimensionsAndPlacementHelper : { menuSceneHeight : Float, containerWidth : Float } -> { above : Float, below : Float } -> { minWidth : Int, maxHeight : Int, placement : Placement }
calculateMenuDimensionsAndPlacementHelper { menuSceneHeight, containerWidth } { above, below } =
    if below < menuSceneHeight && above > below then
        { minWidth = Basics.round containerWidth
        , maxHeight = Basics.min (Basics.round menuSceneHeight) (Basics.round (above - 20))
        , placement = Above
        }

    else
        { minWidth = Basics.round containerWidth
        , maxHeight = Basics.min (Basics.round menuSceneHeight) (Basics.round (below - 20))
        , placement = Below
        }


calculateSpace : Dom.Element -> { above : Float, below : Float }
calculateSpace { viewport, element } =
    { above = element.y - viewport.y
    , below = (viewport.y + viewport.height) - (element.y + element.height)
    }
