module ElmUi.ClearButton exposing (clearButton)

import Element
import Element.Font as Font
import Element.Region as Region
import Select.ElmUi as Select


clearButton : Select.ClearButton msg
clearButton =
    Select.clearButton [ Element.alignRight, Element.centerY, Element.moveLeft 12 ]
        (Element.el [ Font.size 10, Region.description "clear selection" ] (Element.text "❌"))
