module Main exposing (Model, init)

import Array
import Browser
import Browser.Events as Events
import Html exposing (Html, div, text)
import Html.Attributes as HtmlA
import Json.Decode as D
import Random
import Set
import Svg
import Svg.Attributes as SvgA
import Time


cellWidth =
    20


cellHeight =
    20


gridWidthCells =
    20


gridHeightCells =
    20


gamePositions =
    -- Game position matrix in row major order
    List.range 0 (gridWidthCells * gridHeightCells - 1)
        |> List.map
            (\index ->
                let
                    row =
                        index // gridWidthCells

                    col =
                        index - (row * gridHeightCells)
                in
                Position row col
            )


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
    , keyPressHandled : Bool
    , score : Int
    }


type Msg
    = Tick Time.Posix
    | KeyPress Direction
    | PlaceApple Apple
    | Noop


init : () -> ( Model, Cmd Msg )
init _ =
    let
        snake =
            { head = Position 10 10
            , tail = [ Position 10 11, Position 10 12, Position 10 13 ]
            , direction = Up
            }
    in
    ( { snake = snake
      , gameOver = False
      , apple = Nothing
      , keyPressHandled = True
      , score = 0
      }
    , Random.generate PlaceApple (randomValidPosition (snake.head :: snake.tail))
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            updateTick model

        KeyPress direction ->
            if model.keyPressHandled then
                ( { model | snake = updateSnakeDirection model.snake direction, keyPressHandled = False }, Cmd.none )

            else
                ( model, Cmd.none )

        PlaceApple apple ->
            ( { model | apple = apple }, Cmd.none )

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
                Random.generate PlaceApple (randomValidPosition (nextSnake.head :: nextSnake.tail))

            else
                Cmd.none

        nextScore =
            if ateApple then
                model.score + 1

            else
                model.score

        nextModel =
            { model
                | snake = nextSnake
                , gameOver = hasTailCollision
                , apple = nextApple
                , keyPressHandled = True
                , score = nextScore
            }
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


positionToString : Position -> String
positionToString { x, y } =
    String.fromInt x ++ String.fromInt y


randomValidPosition : List Position -> Random.Generator (Maybe Position)
randomValidPosition invalidPositions =
    -- Might be some better way to do the filtering
    -- Elm doesn't suppoer custom comparables so can't do
    -- Set.diff (Set.fromList gamePositions) (Set.fromList invalidPositions) |> Set.toList
    let
        invalidSet =
            invalidPositions |> List.map positionToString |> Set.fromList

        validPositions =
            gamePositions
                |> List.filter (\p -> not <| Set.member (positionToString p) invalidSet)
                |> List.map (\p -> Just p)
    in
    case validPositions of
        [] ->
            Random.constant Nothing

        head :: tail ->
            Random.uniform head tail


subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.gameOver then
        Sub.batch
            [ Time.every 75 Tick
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


viewSnake : Snake -> List (Html Msg)
viewSnake { head, tail } =
    let
        positions =
            head :: tail
    in
    List.map (\p -> viewRect "mediumseagreen" p.x p.y) positions


viewApple : Apple -> List (Html Msg)
viewApple apple =
    case apple of
        Just pos ->
            [ viewRect "red" pos.x pos.y ]

        Nothing ->
            []


viewRect : String -> Int -> Int -> Html Msg
viewRect fillColor row col =
    Svg.rect
        [ SvgA.x (String.fromInt (row * cellWidth))
        , SvgA.y (String.fromInt (col * cellHeight))
        , SvgA.width (String.fromInt cellWidth)
        , SvgA.height (String.fromInt cellHeight)
        , SvgA.fill fillColor
        ]
        []


viewGameOver : Html Msg
viewGameOver =
    Svg.text_
        [ SvgA.x "50%"
        , SvgA.y "50%"
        , SvgA.textAnchor "middle"
        , SvgA.alignmentBaseline "middle"
        , SvgA.fontSize "50"
        , SvgA.fontWeight "bold"
        , SvgA.fill "LightCoral"
        ]
        [ Svg.text "Game Over!" ]


viewTopBar : Model -> Html Msg
viewTopBar model =
    div
        [ HtmlA.style "width" "400px"
        , HtmlA.style "background" "tomato"
        , HtmlA.style "color" "white"
        , HtmlA.style "padding" "10px"
        , HtmlA.style "box-sizing" "border-box"
        ]
        [ div [] [ text ("Score: " ++ String.fromInt model.score) ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewTopBar model
        , Svg.svg
            [ SvgA.width "400"
            , SvgA.height "400"
            , SvgA.viewBox "0 0 400 400"
            , HtmlA.style "background" "black"
            ]
          <|
            List.concat
                [ viewSnake
                    model.snake
                , viewApple model.apple
                , [ if model.gameOver then
                        viewGameOver

                    else
                        Svg.text ""
                  ]
                ]
        ]


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
