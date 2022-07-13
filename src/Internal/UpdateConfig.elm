module Internal.UpdateConfig exposing (UpdateConfig, default)


type alias UpdateConfig =
    { clearInputValueOnBlur : Bool
    , selectExactMatchOnBlur : Bool
    }


default : UpdateConfig
default =
    { clearInputValueOnBlur = False
    , selectExactMatchOnBlur = False
    }
