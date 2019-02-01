module Main exposing (main)

import Browser exposing (Document)
import Css exposing (..)
import Css.Global exposing (body, global)
import DayTime exposing (DayTime, HMS)
import Html.Styled exposing (Attribute, Html, div, img, p, styled, text)
import Html.Styled.Attributes exposing (src)
import Task
import Time


type alias Model =
    { now : DayTime
    , zone : Time.Zone
    , event : DayTime
    }


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | now = DayTime.posixToDayTime model.zone time }, Cmd.none )

        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )


initialModel : () -> ( Model, Cmd Msg )
initialModel flags =
    ( { now = DayTime Time.Sun (HMS 0 0 0)
      , zone = Time.utc
      , event = DayTime Time.Fri (HMS 16 0 0)
      }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , Task.perform AdjustTimeZone Time.here
        ]
    )


getMessage : Model -> String
getMessage { now, event, zone } =
    let
        timeLeftHMS =
            DayTime.diffHMS now.time event.time
    in
    if now.day == event.day then
        if timeLeftHMS.hour > event.time.hour then
            "Generic message about event!"

        else
            DayTime.prettyHMS timeLeftHMS

    else
        DayTime.prettyHMS now.time


timeContainer : List (Attribute msg) -> List (Html msg) -> Html msg
timeContainer =
    styled div
        [ displayFlex
        , justifyContent center
        , alignItems center
        , height (vh 100)
        ]


timeText : List (Attribute msg) -> List (Html msg) -> Html msg
timeText =
    styled p
        [ fontSize (em 10)
        , color (hex "434445")
        , padding (em 1)
        , textAlign center
        ]


mainView : Model -> Html Msg
mainView model =
    timeContainer []
        [ timeText []
            [ text (getMessage model)
            ]
        ]


globalStyle : Html Msg
globalStyle =
    global [ body [ margin zero ] ]


logo : Html Msg
logo =
    styled img [ position absolute, height (px 150), bottom (px 0), margin (px 50), right (px 0) ] [ src "jaydevo.png" ] []


view : Model -> Document Msg
view model =
    { title = "Gong timer!"
    , body =
        List.map Html.Styled.toUnstyled [ globalStyle, mainView model, logo ]
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
