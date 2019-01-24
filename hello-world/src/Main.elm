module Main exposing (main)

import Browser
import Html exposing (..)


type alias Model =
    String


init _ =
    ( "Hello World!", Cmd.none )


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )


subscriptions model =
    Sub.none


view : Model -> Browser.Document msg
view model =
    { title = "Elm"
    , body =
        [ div
            []
            [ text model ]
        ]
    }


main : Program () Model msg
main =
    Browser.document
        { update = update
        , view = view
        , init = init
        , subscriptions = subscriptions
        }
