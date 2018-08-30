module AnimationTest exposing (..)

import Animation exposing (..)
import Expect
import Fuzz
import Test exposing (..)
import Time


animationTests : Test
animationTests =
    describe "animation"
        [ describe "static"
            [ fuzz Fuzz.float "static animations always return the same value" <|
                \t ->
                    static 8 |> animate t |> Expect.equal 8
            ]
        , describe "delay"
            [ fuzz Fuzz.float "animation doesn't start until after delay" <|
                \delayLength ->
                    animation 0
                        |> delay delayLength
                        |> animate delayLength
                        |> Expect.equal 0
            , fuzz Fuzz.float "animation starts after delay" <|
                \delayLength ->
                    animation 0
                        |> delay delayLength
                        |> animate (delayLength + 1)
                        |> Expect.greaterThan 0
            ]
        , describe "timeElapsed"
            [ test "it is 0 for an unstarted, delayed animation" <|
                \_ ->
                    animation 0
                        |> delay 100
                        |> timeElapsed 95
                        |> Expect.equal 0
            , test "it is the time elapsed for a started animation" <|
                \_ ->
                    animation 0
                        |> delay 100
                        |> timeElapsed 105
                        |> Expect.equal 5
            , test "it is the time since start for a finished animation" <|
                \_ ->
                    animation 0
                        |> delay 100
                        |> duration 200
                        |> timeElapsed 305
                        |> Expect.equal 205
            , fuzz Fuzz.float "it is 0 for a static animation" <|
                \clock ->
                    static pi
                        |> timeElapsed clock
                        |> Expect.within (Expect.Absolute 0.0001) 0
            ]
        , describe "timeRemaining"
            [ test "it is the total time for an unstarted animation" <|
                \_ ->
                    animation 0
                        |> delay 100
                        |> duration 200
                        |> timeRemaining 0
                        |> Expect.equal 300
            , test "it is the remaing time for an in-flight animation" <|
                \_ ->
                    animation 0
                        |> duration 200
                        |> timeRemaining 150
                        |> Expect.equal 50
            , test "it is 0 for a complete animation" <|
                \_ ->
                    animation 0
                        |> duration 200
                        |> timeRemaining 200
                        |> Expect.equal 0
            , fuzz Fuzz.float "it is 0 for a static animation" <|
                \clock ->
                    static pi
                        |> timeRemaining clock
                        |> Expect.within (Expect.Absolute 0.0001) 0
            ]
        , describe "isScheduled"
            [ test "true case" <|
                \_ ->
                    animation 0
                        |> isScheduled 0
                        |> Expect.equal True
            , test "true case - delay" <|
                \_ ->
                    animation 0
                        |> delay 100
                        |> isScheduled 100
                        |> Expect.equal True
            , test "false case" <|
                \_ ->
                    animation 0
                        |> isScheduled 0.0001
                        |> Expect.equal False
            ]
        , describe "isRunning"
            [ test "is false before the animation starts" <|
                \_ ->
                    animation 0
                        |> delay 100
                        |> isRunning 100
                        |> Expect.equal False
            , test "is true while the animation runs" <|
                \_ ->
                    animation 0
                        |> duration 100
                        |> isRunning 50
                        |> Expect.equal True
            , test "is false after the animation ends" <|
                \_ ->
                    animation 0
                        |> duration 100
                        |> isRunning 100.00001
                        |> Expect.equal False
            , fuzz Fuzz.float "is false for static animations" <|
                \clock ->
                    static pi
                        |> isRunning clock
                        |> Expect.equal False
            ]
        , describe "isDone"
            [ test "is false before the animation starts" <|
                \_ ->
                    animation 0
                        |> delay 100
                        |> isDone 100
                        |> Expect.equal False
            , test "is false while the animation runs" <|
                \_ ->
                    animation 0
                        |> duration 100
                        |> isDone 50
                        |> Expect.equal False
            , test "is true after the animation ends" <|
                \_ ->
                    animation 0
                        |> duration 100
                        |> isDone 100.00001
                        |> Expect.equal True
            , fuzz Fuzz.float "is true for static animations" <|
                \clock ->
                    static pi
                        |> isDone clock
                        |> Expect.equal True
            ]
        ]
