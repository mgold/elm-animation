module Main exposing (..)

import Color exposing (white, darkBlue)
import Collage
import Element exposing (Element)
import Time exposing (Time)
import Task
import Window
import Mouse
import Html exposing (program)
import AnimationFrame
import Animation exposing (..)


type Msg
    = Tick Time
    | MouseMove Mouse.Position
    | MouseClick Mouse.Position
    | Resize Window.Size
    | NoOp



-- Model = everything persisted between frames. State = state as in a state machine.


type State
    = Entering
    | Here
    | Growing
    | Big
    | Shrinking
    | Exiting
    | Gone


type alias Model =
    { w : Int, h : Int, r : Animation, theta : Animation, clock : Time, state : State }


model0 =
    Model 0 0 (animation 0 |> from 0 |> to here_r) (static (degrees 45)) 0 Entering


dist pos =
    let
        x =
            toFloat pos.x

        y =
            toFloat pos.y
    in
        sqrt <| x * x + y * y


collided pos { r, clock } =
    animate clock r > dist pos


here_r =
    40


big_r =
    70


update : Msg -> Model -> Model
update act model =
    -- this case analysis is ugly but I think it's intrinsic to the component
    case act of
        Tick t ->
            let
                clock =
                    model.clock + t

                radiusDone =
                    isDone clock model.r

                state =
                    case model.state of
                        Entering ->
                            if radiusDone then
                                Here
                            else
                                Entering

                        Growing ->
                            if radiusDone then
                                Big
                            else
                                Growing

                        Shrinking ->
                            if radiusDone then
                                Here
                            else
                                Shrinking

                        Exiting ->
                            if radiusDone then
                                Gone
                            else
                                Exiting

                        Gone ->
                            if isRunning clock model.r then
                                Entering
                            else
                                Gone

                        _ ->
                            model.state
            in
                if model.state == Gone && radiusDone then
                    { model | clock = clock, r = (animation clock |> from 0 |> to here_r |> delay 1500) }
                else
                    { model | clock = clock, state = state }

        MouseMove rawPos ->
            let
                pos =
                    Mouse.Position (rawPos.x - model.w // 2) (model.h // 2 - rawPos.y)

                now =
                    model.clock

                collision =
                    collided pos model

                growingOrBig =
                    model.state == Growing || model.state == Big
            in
                if not collision && growingOrBig then
                    { model
                        | r = undo now model.r
                        , theta = undo now model.theta
                        , state = Shrinking
                    }
                else if collision && model.state == Here then
                    { model
                        | r = retarget now big_r model.r |> duration 150
                        , theta = retarget now (degrees -45) model.theta |> duration 200
                        , state = Growing
                    }
                else
                    model

        MouseClick rawPos ->
            let
                pos =
                    Mouse.Position (rawPos.x - model.w // 2) (model.h // 2 - rawPos.y)
            in
                if collided pos model then
                    { model
                        | r = retarget model.clock 0 model.r |> duration 750
                        , theta = retarget model.clock (degrees 45) model.theta |> duration 750
                        , state = Exiting
                    }
                else
                    model

        Resize { width, height } ->
            { model | w = width, h = height }

        NoOp ->
            model


scene : Model -> Element
scene { w, h, r, theta, clock } =
    let
        radius =
            animate clock r

        angle =
            animate clock theta

        rect =
            Collage.rect (radius / 8) (radius / 1.25) |> Collage.filled white

        circle =
            Collage.circle radius |> Collage.filled darkBlue

        group =
            Collage.group [ circle, rect, rect |> Collage.rotate (degrees 90) ]
                |> Collage.rotate angle
    in
        Collage.collage w h [ group ]


subs : Sub Msg
subs =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.diffs Tick
        , Mouse.clicks MouseClick
        , Mouse.moves MouseMove
        ]


main =
    program
        { init = ( model0, Task.perform Resize Window.size )
        , update = (\msg model -> ( update msg model, Cmd.none ))
        , subscriptions = always subs
        , view = scene >> Element.toHtml
        }
