module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (..)

defaultMargin : Attribute a
defaultMargin = style [("margin", "0 10px")]

activeTabStyle : Attribute a
activeTabStyle = style [("border", "none"), ("width", "100%"), ("height", "3em"), ("backgroundColor", "blue"), ("color", "white")]

inactiveTabStyle : Attribute a
inactiveTabStyle = style [("border", "none"), ("width", "100%"), ("height", "3em"), ("backgroundColor", "white")]

activeTableRowStyle : Attribute a
activeTableRowStyle = style [("backgroundColor", "blue")]
