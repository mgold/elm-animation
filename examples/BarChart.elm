module BarChart exposing (main)

{- This module demonstrates bars that grow at a constant
   speed, even though they have different lengths.
-}

import Animation exposing (..)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Json.Decode exposing (Value)
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes as SA


config =
    { minWidth = 80
    , maxWidth = 960
    , barHeight = 30
    , padding = 20
    , gap = 10
    , bars = 20
    }


randomBars : Generator (List Float)
randomBars =
    Random.list config.bars
        (Random.float
            config.minWidth
            config.maxWidth
        )


animations : List Animation
animations =
    Random.initialSeed 42000
        |> Random.step randomBars
        |> Tuple.first
        |> List.foldl
            (\value animationsList ->
                let
                    previousAnimation =
                        animationsList
                            |> List.head
                            |> Maybe.withDefault (speed 0.5 (static 0))
                in
                (previousAnimation
                    |> to value
                    |> delay (timeRemaining 0 previousAnimation)
                )
                    :: animationsList
            )
            []
        |> List.reverse


viewAnimation : Clock -> Int -> Animation -> Svg Float
viewAnimation clock index anim =
    let
        barWidth =
            round (animate clock anim)

        y =
            config.padding + (config.gap + config.barHeight) * index
    in
    Svg.g []
        [ Svg.rect
            [ SA.fill "lightgray"
            , SA.x (String.fromInt config.padding)
            , SA.y (String.fromInt y)
            , SA.width (String.fromInt barWidth)
            , SA.height (String.fromInt config.barHeight)
            ]
            []
        , Svg.rect
            [ SA.fill "red"
            , SA.x (String.fromInt (config.padding + barWidth))
            , SA.y (String.fromInt y)
            , SA.width <|
                if barWidth == 0 then
                    "0"

                else
                    "1"
            , SA.height (String.fromInt config.barHeight)
            ]
            []
        ]


view : Clock -> Svg Float
view clock =
    let
        width =
            config.padding * 2 + config.maxWidth

        height =
            (config.padding * 2)
                + (config.barHeight * config.bars)
                + (config.gap * (config.bars - 1))
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt height)
        ]
        (List.indexedMap (viewAnimation clock) animations)


main : Program Value Clock Float
main =
    Browser.element
        { init = always ( 0, Cmd.none )
        , update = \dt clock -> ( clock + dt, Cmd.none )
        , subscriptions = \clock -> onAnimationFrameDelta identity
        , view = view
        }
