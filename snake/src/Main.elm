module Main exposing (Model, init)

import Array
import Browser
import Browser.Events as Events
import Html exposing (Html, div)
import Json.Decode as D
import Matrix
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


grid =
    Matrix.repeat 20 20 0


type alias Position =
    { x : Int, y : Int }


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Snake =
    { head : Position
    , tail : List Position
    , direction : Direction
    }


type alias Apple =
    Maybe Position


type alias Model =
    { snake : Snake
    , gameOver : Bool
    , apple : Apple
    }


type Msg
    = Tick Time.Posix
    | KeyPress Direction
    | PlaceApple ( Int, Int )
    | Noop


init : () -> ( Model, Cmd Msg )
init _ =
    ( { snake =
            { head = Position 10 10
            , tail = [ Position 10 11, Position 10 12, Position 10 13 ]
            , direction = Up
            }
      , gameOver = False
      , apple = Nothing
      }
    , Random.generate PlaceApple randomPosition
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            updateTick model

        KeyPress direction ->
            ( { model | snake = updateSnakeDirection model.snake direction }, Cmd.none )

        PlaceApple ( row, col ) ->
            ( { model | apple = Just (Position row col) }, Cmd.none )

        Noop ->
            ( model, Cmd.none )


wrap : Int -> Int
wrap num =
    if num > 19 then
        0

    else if num < 0 then
        19

    else
        num


isValidDirectionChange : Direction -> Direction -> Bool
isValidDirectionChange old new =
    case new of
        Up ->
            old /= Down

        Down ->
            old /= Up

        Left ->
            old /= Right

        Right ->
            old /= Left


updateSnakeDirection : Snake -> Direction -> Snake
updateSnakeDirection snake newDirection =
    let
        { direction } =
            snake

        isValidMove =
            isValidDirectionChange direction newDirection
    in
    if isValidMove then
        { snake | direction = newDirection }

    else
        snake


updateTick : Model -> ( Model, Cmd Msg )
updateTick model =
    let
        { snake, apple } =
            model

        nextHead =
            getNextHead snake.direction snake.head

        ateApple =
            case apple of
                Just position ->
                    isPositionEqual nextHead position

                Nothing ->
                    False

        nextTail =
            getNextTail snake ateApple

        nextSnake =
            Snake nextHead nextTail snake.direction

        hasTailCollision =
            hasCollision nextSnake.head nextSnake.tail

        nextApple =
            if ateApple then
                Nothing

            else
                apple

        nextCmd =
            if ateApple then
                Random.generate PlaceApple randomPosition

            else
                Cmd.none

        nextModel =
            { model | snake = nextSnake, gameOver = hasTailCollision, apple = nextApple }
    in
    ( nextModel, nextCmd )


getNextHead : Direction -> Position -> Position
getNextHead direction position =
    let
        { x, y } =
            position
    in
    case direction of
        Up ->
            { position | y = wrap (y - 1) }

        Down ->
            { position | y = wrap (y + 1) }

        Left ->
            { position | x = wrap (x - 1) }

        Right ->
            { position | x = wrap (x + 1) }


getNextTail : Snake -> Bool -> List Position
getNextTail { head, tail } ateApple =
    if ateApple then
        head :: tail

    else
        head :: List.take (List.length tail - 1) tail


hasCollision : Position -> List Position -> Bool
hasCollision head positions =
    List.any (isPositionEqual head) positions


isPositionEqual : Position -> Position -> Bool
isPositionEqual p1 p2 =
    p1.x == p2.x && p1.y == p2.y


randomPosition : Random.Generator ( Int, Int )
randomPosition =
    Random.pair (Random.int 0 19) (Random.int 0 19)


subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.gameOver then
        Sub.batch
            [ Time.every 100 Tick
            , Events.onKeyDown keyDecoder
            ]

    else
        Sub.none


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
            Noop


viewGrid : Int -> Int -> Int -> Html Msg
viewGrid row col content =
    viewRect "white" row col


viewSnake : Snake -> List (Html Msg)
viewSnake { head, tail } =
    let
        positions =
            head :: tail
    in
    List.map (\p -> viewRect "black" p.x p.y) positions


viewApple : Apple -> List (Html Msg)
viewApple apple =
    case apple of
        Just pos ->
            [ viewRect "red" pos.x pos.y ]

        Nothing ->
            []


viewRect : String -> Int -> Int -> Html Msg
viewRect fillColor row col =
    rect
        [ x (String.fromInt (row * 20))
        , y (String.fromInt (col * 20))
        , width "20"
        , height "20"
        , stroke "black"
        , fill fillColor
        ]
        []


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text
                (if model.gameOver then
                    "Game over"

                 else
                    " "
                )
            ]
        , svg
            [ width "800", height "800", viewBox "0 0 800 800" ]
          <|
            List.concat
                [ Matrix.indexedMap
                    viewGrid
                    grid
                    |> Matrix.toArray
                    |> Array.toList
                , viewSnake
                    model.snake
                , viewApple model.apple
                ]
        ]


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }