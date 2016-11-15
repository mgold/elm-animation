{-
   This demo shows the default behavior of retarget regarding the new speed/duration. An older version had bugs where
   multiple retargets would cause the animation to overshoot by an arbitrary amount, and it would also become slow and
   unresponsive. After contemplating what kind of complex algorithm could find the optimal duration, considering the
   current velocity, direction of current vs. desired travel, timeRemaining, etc, I landed on the idea of retaining the old
   speed. This is very simple, you just have to be sure to convert from duration if that's how it's specified, and works
   remarkably well in terms of keeping the animation responsive, smooth, and not overshooting more than slightly.

   However, if the new destination is very close to the current position, the animation is so short that it seems to stop
   instantaneously. One could convert the average speed to a duration and provide a minimum, perhaps half a second. I tried
   this and it didnt' behave naturally, and besides I don't like hard-coding a minimum time for a duration. Let the client
   do that; I provide a sensible default.

-}


module Main exposing (..)

import Color
import Collage
import Element exposing (Element)
import Time exposing (Time)
import Task
import Window
import Mouse
import Html exposing (program)
import AnimationFrame
import Animation exposing (..)


type alias Model =
    { x : Animation, w : Int, h : Int, clock : Time, dots : List ( Float, Time ) }


model0 =
    Model (animation 0 |> duration 4000) 0 0 0 []


type Msg
    = Tick Time
    | Resize Window.Size
    | Click Mouse.Position
    | Cull
    | NoOp


subs : Sub Msg
subs =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Mouse.clicks Click
        , Window.resizes Resize
        , (Time.every (0.5 * Time.second)) (always Cull)
        ]


update : Msg -> Model -> Model
update act model =
    case act of
        Tick dt ->
            let
                clock =
                    dt + model.clock

                pos =
                    animate clock model.x
            in
                { model | dots = ( pos, clock ) :: model.dots, clock = clock }

        Resize size ->
            { model | w = size.width, h = size.height }

        Click { x } ->
            let
                xPos =
                    toFloat x / toFloat model.w
            in
                { model | x = retarget model.clock xPos model.x }

        Cull ->
            { model | dots = List.take 200 model.dots }

        NoOp ->
            model


render : Model -> Element
render model =
    let
        baseY =
            toFloat model.h / -3

        toX x =
            toFloat model.w * x - toFloat (model.w // 2)

        line =
            Collage.rect (toFloat model.w) 1 |> Collage.filled Color.gray |> Collage.moveY baseY

        dest =
            Collage.square 6 |> Collage.filled Color.red |> Collage.move ( toX <| getTo model.x, baseY )

        circle =
            Collage.circle 8
                |> Collage.filled Color.purple
                |> Collage.move ( toX <| animate model.clock model.x, baseY )

        dots =
            List.map
                (\( x, t ) ->
                    Collage.circle 3
                        |> Collage.filled Color.lightPurple
                        |> Collage.moveX (toFloat model.w * x - toFloat (model.w // 2))
                        |> Collage.moveY (baseY - (t - model.clock) / 10)
                )
                model.dots
    in
        Collage.collage model.w model.h <| line :: dest :: circle :: dots


main =
    program
        { init = ( model0, Task.perform Resize Window.size )
        , update = (\msg model -> ( update msg model, Cmd.none ))
        , subscriptions = always subs
        , view = render >> Element.toHtml
        }
