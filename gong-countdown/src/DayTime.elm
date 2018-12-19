module DayTime exposing (DayTime, HMS, diffHMS, posixToDayTime, posixToHMS, prettyHMS)

import Time


type alias HMS =
    { hour : Int
    , minute : Int
    , second : Int
    }


type alias DayTime =
    { day : Time.Weekday
    , time : HMS
    }


posixToHMS : Time.Zone -> Time.Posix -> HMS
posixToHMS zone time =
    let
        h =
            Time.toHour zone time

        m =
            Time.toMinute zone time

        s =
            Time.toSecond zone time
    in
    HMS h m s


posixToDayTime : Time.Zone -> Time.Posix -> DayTime
posixToDayTime zone time =
    DayTime (Time.toWeekday zone time) (posixToHMS zone time)


diffHMS : HMS -> HMS -> HMS
diffHMS currentTime targetTime =
    let
        s =
            targetTime.second - currentTime.second

        m =
            targetTime.minute
                - currentTime.minute
                - (if s < 0 then
                    1

                   else
                    0
                  )

        h =
            targetTime.hour
                - currentTime.hour
                - (if m < 0 then
                    1

                   else
                    0
                  )
    in
    HMS (modBy 24 h) (modBy 60 m) (modBy 60 s)


prettyHMS : HMS -> String
prettyHMS { hour, minute, second } =
    [ hour, minute, second ]
        |> List.map (String.fromInt >> String.padLeft 2 '0')
        |> List.intersperse ":"
        |> List.foldr (++) ""
