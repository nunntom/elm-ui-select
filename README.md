# Elm Ui Select

A select widget for elm-ui.

## Features:

- Filter menu items based on user input.
- Keyboard selection with up/down/PgUp/PgDn keys. Menu scrolls with selection.
- Automatically attempts to size and place the menu based on the position of the input in the viewport.
- Customisable: Supply your own attributes for the input, menu or your own option Element.
- Make HTTP requests to retrieve matching options (The package does not make the requests, but you supply the Cmd/Effect - no elm/http dependency)
- Can be used with the Effect pattern

## Example

```elm

-- MODEL

type alias Model =
    { countrySelect : Select Country
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { countrySelect =
            Select.init "country-select"
                |> Select.setItems Countries.all
      }
    , Cmd.none
    )



-- UPDATE

type Msg
    = CountrySelectMsg (Select.Msg Country)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DropdownMsg subMsg ->
            Select.update DropdownMsg subMsg model.countrySelect
                |> Tuple.mapFirst (\select -> { model | countrySelect = select })

-- VIEW

view : Model -> Element Msg
view model =
    Select.view []
        { onChange = DropdownMsg
        , label = Input.labelAbove [] (text "Choose a country")
        , placeholder = Just (Input.placeholder [] (text "Type to search"))
        , itemToString = \c -> c.flag ++ " " ++ c.name
        }
        |> Select.toElement model.countrySelect

```

## Limitations

There are issues when the input is placed within a parent element that has overflow scroll or auto: the dropdown may be clipped by the parent. This can be overcome by adding `Element.htmlAttribute (Html.Attribute.style "position" "fixed")` as an attribute to the menu, but if the parent also has a transform applied, it gets clipped again. This means any parent with e.g. Element.scrollBarY + Element.moveDown can cause issues. This is due to [a feature of the current CSS spec](https://bugs.chromium.org/p/chromium/issues/detail?id=20574).
