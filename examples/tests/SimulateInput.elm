module SimulateInput exposing (arrowDown, enter)

import Html.Attributes
import Json.Encode as Encode
import ProgramTest exposing (ProgramTest)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


arrowDown : String -> (ProgramTest model msg effect -> ProgramTest model msg effect)
arrowDown =
    simulateKey "ArrowDown"


enter : String -> (ProgramTest model msg effect -> ProgramTest model msg effect)
enter =
    simulateKey "Enter"


simulateKey : String -> String -> (ProgramTest model msg effect -> ProgramTest model msg effect)
simulateKey key id =
    ProgramTest.simulateDomEvent (Query.find [ Selector.attribute (Html.Attributes.id (id ++ "-input")) ])
        ( "keydown", Encode.object [ ( "key", Encode.string key ) ] )
