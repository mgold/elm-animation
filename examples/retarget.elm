import Animation exposing (..)
import Time exposing (Time)

import Mouse
import Window
import Graphics.Collage as Collage
import Color exposing (blue, red, purple)
import Graphics.Element exposing (Element)
import Debug

type Action = Tick Time | Interrupt | NoOp
type alias State = { clock : Time, x : Animation, prevs : List Float}
state0 = State 0 (animation 0 |> from -300 |> to 300 |> duration 3000) []

step : Action -> State -> State
step act state =
    case act of
        Tick t -> {state| clock <- t + state.clock,
                          prevs <- Debug.watch "x" (animate state.clock state.x) :: state.prevs}
                  |> Debug.watchSummary "clock" (always state.clock)
        Interrupt -> {state| x <- retarget state.clock 100 state.x}
                    |> Debug.watchSummary "interrupted" (always state.clock)
        NoOp -> state

interrupts = Signal.foldp (\_ c -> c+1) 0 (Time.fps 10)
    |> Signal.map (\c -> if c == 13 then Interrupt else NoOp)

ticks = Signal.map Tick (Time.fps 60)

actions : Signal Action
actions = Signal.merge ticks interrupts

state : Signal State
state = Signal.foldp step state0 actions

render : State -> Element
render {clock, x, prevs} =
    let xPos = animate clock x
        circle = Collage.circle 20 |> Collage.filled blue |> Collage.moveX xPos
        circles = List.indexedMap (\i x -> Collage.circle 2 |> Collage.filled blue |> Collage.move (x, toFloat -i)) prevs
    in Collage.collage 700 600 <| circle :: circles

main = Signal.map render (Signal.sampleOn ticks state)
