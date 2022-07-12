module Internal.Filter exposing
    ( Filter
    , contains
    , custom
    , customWithSort
    , filterOptions
    , startsWith
    , startsWithThenContains
    )

import Internal.Option as Option exposing (Option)


type Filter a
    = Filter (String -> List (Option a) -> List (Option a))


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
            List.filter (Option.toItem >> f inputValue)
        )


customWithSort : (String -> a -> Maybe Int) -> Filter a
customWithSort toScore =
    Filter
        (\inputValue ->
            List.filterMap (\opt -> Maybe.map (Tuple.pair opt) (toScore inputValue (Option.toItem opt)))
                >> List.sortBy Tuple.second
                >> List.map Tuple.first
        )


stringFilter : (String -> String -> Bool) -> String -> List (Option a) -> List (Option a)
stringFilter f inputValue options =
    List.filter
        (\option ->
            f (String.toLower inputValue) (String.toLower (Option.toString option))
        )
        options


filterOptions : String -> Maybe (Filter a) -> List (Option a) -> List (Option a)
filterOptions inputValue filter options =
    if inputValue /= "" then
        Maybe.map (\(Filter f) -> f inputValue options) filter
            |> Maybe.withDefault options

    else
        options
