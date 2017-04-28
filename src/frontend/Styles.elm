module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (..)

activeTabStyle : Attribute a
activeTabStyle = style [("border", "none"), ("width", "100%"), ("height", "3em"), ("backgroundColor", "blue"), ("color", "white")]

inactiveTabStyle : Attribute a
inactiveTabStyle = style [("border", "none"), ("width", "100%"), ("height", "3em"), ("backgroundColor", "white")]

activeTableRowStyle : Attribute a
activeTableRowStyle = style [("backgroundColor", "blue")]

tableStyle : Attribute a
tableStyle = style [("width", "100%"), ("borderCollapse", "collapse"), ("overflow-x", "hidden"), ("marginBottom", "1em")]
