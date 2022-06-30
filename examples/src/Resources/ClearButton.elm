module Resources.ClearButton exposing (clearButton)

import Element
import Element.Font as Font
import Html.Attributes
import Select


clearButton : Select.ClearButton msg
clearButton =
    Select.clearButton [ Element.alignRight, Element.centerY, Element.moveLeft 12 ]
        (Element.el [ Font.size 10, Element.htmlAttribute (Html.Attributes.title "clear selection") ] (Element.text "❌"))
