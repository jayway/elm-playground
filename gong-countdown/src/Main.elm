module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation
import Css exposing (..)
import Css.Global exposing (body, global)
import DayTime exposing (HMS)
import Html.Styled exposing (Attribute, Html, div, img, p, styled, text)
import Html.Styled.Attributes exposing (src)
import Task
import Time
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query as Query exposing (Parser)


type alias Model =
    { now : HMS
    , zone : Time.Zone
    , event : HMS
    , message : String
    }


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | now = DayTime.posixToDayTime model.zone time }, Cmd.none )

        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        Noop ->
            ( model, Cmd.none )


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        parsedUrl =
            parseUrl url |> Maybe.withDefault (ParsedQuery "The gong was struck" 16 0)
    in
    ( { now = HMS 0 0 0
      , zone = Time.utc
      , event = HMS parsedUrl.hour parsedUrl.minute 0
      , message = parsedUrl.message
      }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , Task.perform AdjustTimeZone Time.here
        ]
    )


type alias ParsedQuery =
    { message : String, hour : Int, minute : Int }


messageParser : Parser String
messageParser =
    Query.map (Maybe.withDefault "nonsense") (Query.string "message")


intParser : Int -> String -> Parser Int
intParser default key =
    Query.map (Maybe.withDefault default) (Query.int key)


parser : Parser ParsedQuery
parser =
    Query.map3 ParsedQuery messageParser (intParser 16 "hour") (intParser 0 "minute")


parseUrl : Url -> Maybe ParsedQuery
parseUrl url =
    Url.Parser.parse (Url.Parser.query parser) url



-- ?message="Ber"&hour=16&minute=00


getMessage : Model -> String
getMessage { now, event, zone, message } =
    let
        timeLeftHMS =
            DayTime.diffHMS now event
    in
    if timeLeftHMS.hour > event.hour then
        message

    else
        DayTime.prettyHMS timeLeftHMS


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


gong : Html Msg
gong =
    styled img [ position absolute, height (px 300), bottom (px 0), margin (px 50), left (px 0) ] [ src "gong.png" ] []


stick : Html Msg
stick =
    styled img [ position absolute, height (px 150), bottom (px 0), margin (px 50), left (px 100) ] [ src "stick.png" ] []


view : Model -> Document Msg
view model =
    { title = "Gong timer!"
    , body =
        List.map Html.Styled.toUnstyled [ globalStyle, mainView model, logo, gong, stick ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> Noop
        , onUrlChange = \_ -> Noop
        }
