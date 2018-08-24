module AnimationTest exposing (..)

import Animation exposing (..)
import Expect
import Fuzz
import Test exposing (..)
import Time


animationTests : Test
animationTests =
    describe "animation"
        [ fuzz Fuzz.float "static animations always return the same value" <|
            \t ->
                static 8 |> animate t |> Expect.equal 8
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
        ]
