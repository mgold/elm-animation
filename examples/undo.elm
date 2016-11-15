module Main exposing (..)

{- This example demonstrates the need to set the easing function of undone animations as `(\t -> 1 - (a.ease (1 - t)))`.
   Otherwise, undone animations will appear to work for symmetrical easing functions but not for asymmetric ones. The
   outer 1 - is necessary so that the from and to values may be set correctly, as they may be inspected.
-}

import Color
import Collage
import Element exposing (Element)
import Time exposing (Time)
import Task
import AnimationFrame
import Window
import Html exposing (program)
import Animation exposing (..)


{-| asymmetric ease in cubic
-}
myEase x =
    x * x * x


type alias Model =
    { w : Int, h : Int, x : Animation, clock : Time, undone : Bool, dotsOut : List Float, dotsBack : List Float }


type Msg
    = Resize Window.Size
    | Tick Time
    | NoOp


model0 =
    Model 0 0 (animation 0 |> duration 4000 |> ease myEase) 0 False [] []


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            let
                clock =
                    model.clock + dt

                pos =
                    animate clock model.x
            in
                if not model.undone && pos > 0.999 then
                    { model | x = undo clock model.x, undone = True, dotsOut = pos :: model.dotsOut, dotsBack = [ pos ], clock = clock }
                else if not model.undone then
                    { model | dotsOut = pos :: model.dotsOut, clock = clock }
                else
                    { model | dotsBack = pos :: model.dotsBack, clock = clock }

        Resize { width, height } ->
            { model | w = width, h = height }

        NoOp ->
            model


scene : Model -> Element
scene model =
    let
        circle =
            Collage.circle 8
                |> Collage.filled Color.purple
                |> Collage.move
                    ( toFloat model.w * animate model.clock model.x - toFloat (model.w // 2)
                    , if model.undone then
                        -30
                      else
                        0
                    )

        dotsOut =
            List.map
                (\x ->
                    Collage.circle 3
                        |> Collage.filled Color.lightPurple
                        |> Collage.moveX (toFloat model.w * x - toFloat (model.w // 2))
                )
                model.dotsOut

        dotsBack =
            List.map
                (\x ->
                    Collage.circle 3
                        |> Collage.filled Color.darkPurple
                        |> Collage.move ( toFloat model.w * x - toFloat (model.w // 2), -30 )
                )
                model.dotsBack
    in
        Collage.collage model.w model.h <| circle :: dotsOut ++ dotsBack


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
