module Internal.Filter exposing
    ( Filter
    , contains
    , custom
    , customWithSort
    , filterOptions
    , startsWith
    , startsWithThenContains
    )


type Filter a
    = Filter (String -> List ( a, String ) -> List ( a, String ))


startsWith : Filter a
startsWith =
    stringFilter String.startsWith
        |> Filter


contains : Filter a
contains =
    stringFilter String.contains
        |> Filter


startsWithThenContains : Filter a
startsWithThenContains =
    Filter
        (\inputValue options ->
            List.append
                (stringFilter String.startsWith inputValue options)
                (stringFilter (\s input -> (not <| String.startsWith s input) && String.contains s input) inputValue options)
        )


custom : (String -> a -> Bool) -> Filter a
custom f =
    Filter
        (\inputValue ->
            List.filter (\( a, _ ) -> f inputValue a)
        )


customWithSort : (String -> a -> Maybe Int) -> Filter a
customWithSort toScore =
    Filter
        (\inputValue ->
            List.filterMap (\opt -> Maybe.map (Tuple.pair opt) (toScore inputValue (Tuple.first opt)))
                >> List.sortBy Tuple.second
                >> List.map Tuple.first
        )


stringFilter : (String -> String -> Bool) -> String -> List ( a, String ) -> List ( a, String )
stringFilter f inputValue options =
    List.filter
        (\( _, s ) ->
            f (String.toLower inputValue) (String.toLower s)
        )
        options


filterOptions : String -> Maybe (Filter a) -> List ( a, String ) -> List ( a, String )
filterOptions inputValue filter options =
    if inputValue /= "" then
        Maybe.map (\(Filter f) -> f inputValue options) filter
            |> Maybe.withDefault options

    else
        options
