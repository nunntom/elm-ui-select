# Elm-Ui Select

A select widget for elm-ui with keyboard input/scrolling, filtering and requests.

## Features:

- Filter menu items based on user input.
- Keyboard selection with up/down/PgUp/PgDn keys. Menu scrolls with selection.
- Automatically attempts to size and place the menu based on the position of the input in the viewport.
- Customisable: Supply your own attributes for the input, menu or your own option Element.
- Make HTTP requests to retrieve matching options (The package does not make the requests, but you supply the Cmd/Effect - no elm/http dependency)
- Can be used with the Effect pattern and [elm-program-test](https://package.elm-lang.org/packages/avh4/elm-program-test/4.0.0/) to simulate input to the select.

### [View a live interactive demo](https://nunntom.github.io/elm-ui-select/)

## Example

```elm

-- MODEL

type alias Model =
    { countrySelect : Select Country }


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
    Select.view
        |> Select.toElement []
            { select = model.countrySelect
            , onChange = CountrySelectMsg
            , itemToString = \c -> c.flag ++ " " ++ c.name
            , label = Input.labelAbove [] (text "Choose a country")
            , placeholder = Just (Input.placeholder [] (text "Type to search"))
            }

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
    Select.view
        |> Select.toElement []
            { onChange = CountrySelectMsg
            , select = Select.setItems countries model.countrySelect
            , label = Input.labelAbove [] (text "Choose a country")
            , placeholder = Just (Input.placeholder [] (text "Type to search"))
            , itemToString = \c -> c.flag ++ " " ++ c.name
            }


```

## Limitations

There are issues when the input is placed within a parent element that has overflow scroll or auto: the menu may be clipped by the parent. This can be overcome by using [Select.withMenuPositionFixed](https://package.elm-lang.org/packages/nunntom/elm-ui-select/4.0.0/Select/#withMenuPositionFixed), but if the parent also has a transform applied, it gets clipped again. This means any parent with e.g. Element.scrollBarY + Element.moveDown/moveLeft etc. can cause issues. This is due to [a feature of the current CSS spec](https://bugs.chromium.org/p/chromium/issues/detail?id=20574).
