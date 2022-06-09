module Select.Filter exposing (Filter, contains, customFilter, startsWith, startsWithThenContains)

import Internal.Filter


type alias Filter a =
    Internal.Filter.Filter a


startsWith : Filter a
startsWith =
    Internal.Filter.startsWith


contains : Filter a
contains =
    Internal.Filter.contains


startsWithThenContains : Filter a
startsWithThenContains =
    Internal.Filter.startsWithThenContains


customFilter : (String -> a -> Bool) -> Filter a
customFilter =
    Internal.Filter.custom
