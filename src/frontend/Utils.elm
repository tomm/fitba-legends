module Utils exposing (dateFormat, timeFormat)

import Time
import Date

dateFormat : Date.Date -> String
dateFormat d =
    (Date.dayOfWeek d |> toString)
    ++ " " ++
    (Date.day d |> toString)
    ++ " " ++
    (Date.month d |> toString)
    ++ " " ++
    (Date.hour d |> toString)
    ++ ":" ++
    (Date.minute d |> toString)

timeFormat : Time.Time -> String
timeFormat t = Date.fromTime t |> dateFormat
