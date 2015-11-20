module BarChart where

{-| This module demonstrates bars that grow at a constant speed, even though they have different lengths.
-}

import Color exposing (gray, red)
import Graphics.Element exposing (Element, flow, down, right, spacer, color)
import Time exposing (Time)
import Random

import Animation exposing (..)

data : List Int
data =
    let gen = Random.int 1 12 |> Random.list 20
        seed = Random.initialSeed 42000
    in Random.generate gen seed |> fst

animations : List Animation
animations =
    List.scanl
        (\val prev -> prev |> to val |> delay (timeRemaining 0 prev))
        (static 0 |> speed 0.5)
        (List.map (\x -> toFloat x * 80) data) -- size up to bar length now for smoother animation

clock = Signal.foldp (+) 0 (Time.fps 20)

render1 : Int -> Element
render1 x =
    flow down
        [ spacer 1 10
        , flow right
            [ spacer x 30 |> color gray
            , spacer (if x == 0 then 0 else 1) 30 |> color red
            ]
        ]

render : List Animation -> Time -> Element
render anims t =
    flow right
        [spacer 40 1
        , flow down <| List.map ((animate t)>>round>>render1) anims
        ]

main = Signal.map (render animations) clock
