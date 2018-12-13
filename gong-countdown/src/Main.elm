module Main exposing (Model, initialModel, main)

import Browser exposing (Document)
import Html exposing (text)
import Task
import Time


type alias Model =
    { currentTime : HMS
    , zone : Time.Zone
    , eventTime : HMS
    }


type alias HMS =
    { hour : Int
    , minute : Int
    , second : Int
    }


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | currentTime = posixToHMS model.zone time }, Cmd.none )

        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )


initialModel : () -> ( Model, Cmd Msg )
initialModel flags =
    ( { currentTime = HMS 0 0 0
      , zone = Time.utc
      , eventTime = HMS 16 0 0
      }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , Task.perform AdjustTimeZone Time.here
        ]
    )


prettyDate : Time.Zone -> Time.Posix -> String
prettyDate zone time =
    let
        m =
            time
                |> Time.toMinute zone
                |> String.fromInt

        h =
            time
                |> Time.toHour zone
                |> String.fromInt
    in
    h ++ ":" ++ m


normaliseTime : Int -> Int -> Int
normaliseTime max value =
    if value < 0 then
        max + value

    else
        value


timeLeft : HMS -> HMS -> HMS
timeLeft currentTime targetTime =
    let
        s =
            targetTime.second - currentTime.second

        normalizedSecond =
            normaliseTime 60 s

        m =
            targetTime.minute
                - currentTime.minute
                - (if s < 0 then
                    1

                   else
                    0
                  )

        normalizedMinute =
            normaliseTime 60 m

        h =
            targetTime.hour
                - currentTime.hour
                - (if m < 0 then
                    1

                   else
                    0
                  )

        normalizedHour =
            normaliseTime 24 h
    in
    HMS normalizedHour normalizedMinute normalizedSecond


prettyHMS : HMS -> String
prettyHMS { hour, minute, second } =
    [ hour, minute, second ]
        |> List.map String.fromInt
        |> List.intersperse ":"
        |> List.foldr (++) ""


view : Model -> Document Msg
view { zone, currentTime, eventTime } =
    { title = "Gong timer!"
    , body =
        [ Html.div [] [ text (prettyHMS (timeLeft currentTime eventTime)) ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


main =
    Browser.document
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
