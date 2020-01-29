module Main exposing (..)

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
import Collage
import Element exposing (Element)
import Time exposing (Time)
import Task exposing (Task)
import AnimationFrame
import Window
import Html exposing (program)
import Animation exposing (..)


type alias Model =
    { r : Animation, x : Animation, y : Animation, w : Int, h : Int, clock : Time }


model0 =
    Model (animation 0 |> from 40 |> to 60 |> duration (0.2 * Time.second))
        (animation 0 |> from 200 |> to -200 |> duration Time.second)
        (animation 0 |> from 200 |> to -200 |> duration Time.second |> delay Time.second)
        0
        0
        0


type Msg
    = Tick Time
    | Resize Window.Size
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            let
                clock =
                    model.clock + dt

                r =
                    if isDone clock model.r then
                        undo clock model.r
                    else
                        model.r

                moveDone =
                    isDone clock model.x && isDone clock model.y

                x =
                    if moveDone then
                        undo clock model.x
                    else
                        model.x

                y =
                    if moveDone then
                        undo clock model.y |> delay Time.second
                    else
                        model.y
            in
                { model | clock = clock, r = r, x = x, y = y }

        Resize { width, height } ->
            { model | w = width, h = height }

        NoOp ->
            model


scene : Model -> Element
scene { w, h, r, x, y, clock } =
    let
        radius =
            animate clock r

        pos =
            ( animate clock x, animate clock y )

        circle =
            Collage.circle radius |> Collage.filled yellow |> Collage.move pos
    in
        Collage.collage w h [ circle ]


subs : Sub Msg
subs =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.diffs Tick
        ]


main =
    program
        { init = ( model0, Task.perform Resize Window.size )
        , update = (\msg model -> ( update msg model, Cmd.none ))
        , subscriptions = always subs
        , view = scene >> Element.toHtml
        }
