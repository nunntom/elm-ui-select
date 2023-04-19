module Internal.ViewConfig exposing (ViewConfigInternal, init, isMobile, shouldShowNoMatchElement, toFilteredOptions, toPlacement)

import Internal.Filter as Filter exposing (Filter)
import Internal.Model as Model exposing (Model)
import Internal.Option exposing (Option)
import Internal.OptionState exposing (OptionState)
import Internal.Placement exposing (Placement)


type alias ViewConfigInternal a attribute view =
    { filter : Maybe (Filter a)
    , menuPlacement : Maybe Placement
    , menuMaxHeight : Maybe Int
    , menuMaxWidth : Maybe Int
    , menuAttributes : List (Placement -> List attribute)
    , noMatchElement : Maybe view
    , optionElement : Maybe (OptionState -> a -> view)
    , clearButton : Maybe ( List attribute, view )
    , positionFixed : Bool
    , clearInputValueOnBlur : Bool
    , selectExactMatchOnBlur : Bool
    , selectOnTab : Bool
    , minInputLength : Maybe Int
    , openOnFocus : Bool
    , mobileBreakpoint : Maybe Float
    , before : Maybe view
    , after : Maybe view
    }


init : ViewConfigInternal a attribute view
init =
    { filter = Just Filter.startsWithThenContains
    , menuPlacement = Nothing
    , menuMaxHeight = Nothing
    , menuMaxWidth = Nothing
    , menuAttributes = []
    , noMatchElement = Nothing
    , optionElement = Nothing
    , clearButton = Nothing
    , positionFixed = False
    , clearInputValueOnBlur = False
    , selectExactMatchOnBlur = False
    , selectOnTab = True
    , minInputLength = Nothing
    , openOnFocus = True
    , mobileBreakpoint = Just 760
    , before = Nothing
    , after = Nothing
    }


shouldShowNoMatchElement : List (Option a) -> Model a -> ViewConfigInternal a attribute view -> Bool
shouldShowNoMatchElement filteredOptions select viewConfig =
    List.length filteredOptions
        == 0
        && Model.isOpen select
        && (String.length (Model.toInputValue select) >= Maybe.withDefault 1 viewConfig.minInputLength)
        && (Model.toRequestState select == Nothing || Model.isRequestSuccess select)


toPlacement : Model a -> ViewConfigInternal a attribute view -> Placement
toPlacement select viewConfig =
    Model.toMenuPlacement viewConfig.menuMaxHeight viewConfig.menuPlacement select


toFilteredOptions : Model a -> (a -> String) -> ViewConfigInternal a attribute view -> List (Option a)
toFilteredOptions select itemToString viewConfig =
    Model.toFilteredOptions True viewConfig.minInputLength itemToString viewConfig.filter select


isMobile : Model a -> ViewConfigInternal a attribute view -> Bool
isMobile select { mobileBreakpoint } =
    Maybe.map2 (\w bp -> w <= bp) (Model.toViewportWidth select) mobileBreakpoint
        |> Maybe.withDefault False
