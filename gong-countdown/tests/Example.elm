module Example exposing (suite)

import DayTime exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "DayTime module"
        [ describe "diffHMS"
            [ test "returns the correct time when all time units are negative" <|
                \_ ->
                    let
                        currentTime =
                            HMS 17 1 1

                        targetTime =
                            HMS 16 0 0

                        expected =
                            HMS 22 58 59
                    in
                    Expect.equal expected (DayTime.diffHMS currentTime targetTime)
            ]
        ]
