{- This example shows a looping animation using only the undo method, not retarget. The step function is a little more
repetitive than I'd like, suggesting a loop function. Without a major overhaul, the best implementation would be

    loop : Time -> Animation -> Animation
    loop t a = if isDone t a then undo t a else a

This requires the client to call the function on each invocation of update, which I consider unacceptable. Better to
have this be handled under the covers, but that means expanding the Animation union type. The best way to do that is
probably define a StandardAnimation type and have all tags convert to it. Alternatively, come up with a sufficiently
general representation and hope it isn't too crazy to work with.

Or stick it in a separate module - how often can you see multiple animations into the future? The trend seems to be the
reverse direction, with physics simulations seeing only the next frame, handling interactions as they come rather than
interrupting a plan. In the mean time, animations are certainly composable if the client does some of the work
themselves.

End brain dump.

-}
import Color exposing (yellow)
import Graphics.Collage as Collage
import Graphics.Element exposing (Element)
import Time exposing (Time)
import Window

import Animation exposing (..)

type alias Model = { r : Animation, x : Animation, y : Animation, clock : Time }
model0 = Model (animation 0 |> from 40 |> to 60 |> duration (0.2*Time.second))
               (animation 0 |> from 200 |> to -200 |> duration Time.second)
               (animation 0 |> from 200 |> to -200 |> duration Time.second |> delay Time.second)
               0

step : Time -> Model -> Model
step dt model =
    let clock = model.clock + dt
        r = if isDone clock model.r then undo clock model.r else model.r
        moveDone = isDone clock model.x && isDone clock model.y
        x = if moveDone then undo clock model.x else model.x
        y = if moveDone then undo clock model.y |> delay Time.second else model.y
    in {model| clock = clock, r = r, x = x, y = y}

model : Signal Model
model = Signal.foldp step model0 (Time.fps 60)

render : (Int, Int) -> Model -> Element
render (w, h) {r, x, y, clock} =
    let radius = animate clock r
        pos = (animate clock x, animate clock y)
        circle = Collage.circle radius |> Collage.filled yellow |> Collage.move pos
    in Collage.collage w h [circle]

main = Signal.map2 render Window.dimensions model
