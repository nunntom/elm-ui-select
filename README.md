# Elm-Ui Select

A select/dropdown widget for elm-ui.

## Features:

- Filter menu items based on user input.
- Keyboard selection with up/down/PgUp/PgDn keys. Menu scrolls with selection.
- Automatically attempts to size and place the menu based on the position of the input in the viewport.
- Customisable: Supply your own attributes for the input, menu or your own option Element.
- Make HTTP requests to retrieve matching options (The package does not make the requests, but you supply the Cmd/Effect - no elm/http dependency)
- Can be used with the Effect pattern and [elm-program-test](https://package.elm-lang.org/packages/avh4/elm-program-test/3.6.3/) to simulate input to the select.

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
        CountrySelectMsg subMsg ->
            Select.update CountrySelectMsg subMsg model.countrySelect
                |> Tuple.mapFirst (\select -> { model | countrySelect = select })

-- VIEW

view : Model -> Element Msg
view model =
    Select.view []
        { onChange = CountrySelectMsg
        , label = Input.labelAbove [] (text "Choose a country")
        , placeholder = Just (Input.placeholder [] (text "Type to search"))
        , itemToString = \c -> c.flag ++ " " ++ c.name
        }
        |> Select.toElement model.countrySelect

```

Note that if you have the list of items in your model already, you can also pass them directly in the view instead of setting the items in init and it will work just as well:

```elm


init : ( PageModel, Cmd Msg )
init =
    ( { countrySelect = Select.init "country-select" }
    , Cmd.none
    )


view : List Country -> PageModel -> Element Msg
view countries model =
    Select.view []
        { onChange = CountrySelectMsg
        , label = Input.labelAbove [] (text "Choose a country")
        , placeholder = Just (Input.placeholder [] (text "Type to search"))
        , itemToString = \c -> c.flag ++ " " ++ c.name
        }
        |> Select.toElement (Select.setItems countries model.countrySelect)


```

## Limitations

There are issues when the input is placed within a parent element that has overflow scroll or auto: the menu may be clipped by the parent. This can be overcome by using [Select.withMenuPositionFixed](https://package.elm-lang.org/packages/nunntom/elm-ui-select/2.0.3/Select/#withMenuPositionFixed), but if the parent also has a transform applied, it gets clipped again. This means any parent with e.g. Element.scrollBarY + Element.moveDown/moveLeft etc. can cause issues. This is due to [a feature of the current CSS spec](https://bugs.chromium.org/p/chromium/issues/detail?id=20574).
