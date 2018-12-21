module Main exposing (Model, init)

import Array
import Browser
import Browser.Events as Events
import Html exposing (Html)
import Json.Decode as D
import Matrix
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


grid =
    Matrix.repeat 20 20 0


type alias Model =
    { snake : Snake
    }


type Msg
    = Tick Time.Posix
    | KeyPress Direction


init : () -> ( Model, Cmd msg )
init _ =
    ( { snake = { position = [ Position 10 10, Position 10 11 ], direction = Down } }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | snake = updateSnakePosition model.snake }, Cmd.none )

        KeyPress direction ->
            ( { model | snake = updateSnakeDirection model.snake direction }, Cmd.none )


wrap : Int -> Int
wrap num =
    if num > 19 then
        0

    else if num < 0 then
        19

    else
        num


updateSnakeDirection : Snake -> Direction -> Snake
updateSnakeDirection snake direction =
    case direction of
        Unknown ->
            snake

        _ ->
            { snake | direction = direction }


updateSnakePosition : Snake -> Snake
updateSnakePosition snake =
    -- TODO: Given [Tail, Body..., Head] drop Tail NewTailAndBody = Body + Head new Head = according to direction
    case snake.direction of
        Up ->
            { snake | position = List.map (\p -> { p | y = wrap (p.y - 1) }) snake.position }

        Down ->
            { snake | position = List.map (\p -> { p | y = wrap (p.y + 1) }) snake.position }

        Right ->
            { snake | position = List.map (\p -> { p | x = wrap (p.x + 1) }) snake.position }

        Left ->
            { snake | position = List.map (\p -> { p | x = wrap (p.x - 1) }) snake.position }

        Unknown ->
            snake


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 100 Tick
        , Events.onKeyDown keyDecoder
        ]


keyDecoder : D.Decoder Msg
keyDecoder =
    D.map toDirection (D.field "key" D.string)


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" ->
            KeyPress Left

        "ArrowRight" ->
            KeyPress Right

        "ArrowUp" ->
            KeyPress Up

        "ArrowDown" ->
            KeyPress Down

        _ ->
            KeyPress Unknown


viewGrid : List Position -> Int -> Int -> Int -> Html Msg
viewGrid snakePositions row col content =
    let
        hasSnake =
            List.any (\p -> p.x == row && p.y == col) snakePositions

        color =
            if hasSnake then
                "black"

            else
                "white"
    in
    rect [ x (String.fromInt (row * 20)), y (String.fromInt (col * 20)), width "20", height "20", stroke "black", fill color ] []


type alias Position =
    { x : Int, y : Int }


type Direction
    = Up
    | Down
    | Left
    | Right
    | Unknown


type alias Snake =
    { position : List Position
    , direction : Direction
    }


view : Model -> Html Msg
view model =
    svg
        [ width "800", height "800", viewBox "0 0 800 800" ]
        (Matrix.indexedMap
            (viewGrid model.snake.position)
            grid
            |> Matrix.toArray
            |> Array.toList
        )


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
