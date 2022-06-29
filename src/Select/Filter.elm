module Select.Filter exposing
    ( Filter
    , startsWith, contains, startsWithThenContains, custom, customWithSort
    )

{-| Filter the list of options based on the input value


# Type

@docs Filter


# Filters

@docs startsWith, contains, startsWithThenContains, custom, customWithSort

-}

import Internal.Filter


{-| A Filter
-}
type alias Filter a =
    Internal.Filter.Filter a


{-| Keep all options whose string value starts with the input value (case insensitive).
-}
startsWith : Filter a
startsWith =
    Internal.Filter.startsWith


{-| Keep all options whose string value contains the input value (case insensitive).
-}
contains : Filter a
contains =
    Internal.Filter.contains


{-| Keep all options whose string value starts with or contains the input value (case insensitive).
The items that start with the value appear at the top.
-}
startsWithThenContains : Filter a
startsWithThenContains =
    Internal.Filter.startsWithThenContains


{-| Create a simple custom filter. Keep all values that satisfy the test.
-}
custom : (String -> a -> Bool) -> Filter a
custom =
    Internal.Filter.custom


{-| Create a custom filter that sorts. Keep all values that return a maybe and sort in ascending order based on the score.
-}
customWithSort : (String -> a -> Maybe Int) -> Filter a
customWithSort =
    Internal.Filter.customWithSort
