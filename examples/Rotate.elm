module Rotate exposing (main)

{- This example shows animating to an angle.
   The only slightly tricky bit is making sure
   you go the shortest way around.
-}

import Animation exposing (..)
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onClick, onResize)
import Collage
import Color
import Element exposing (Element)
import Html exposing (program)
import Json.Decode as Decode exposing (Decoder)
import Task


type alias Model =
    { w : Int
    , h : Int
    , theta : Animation
    , r : Animation
    , clock : Clock
    }


model0 =
    Model 0 0 (static 0) (static 100) 0


type Msg
    = Tick Float
    | Click Int Int
    | Resize Int Int
    | NoOp


update : Msg -> Model -> Model
update act model =
    case act of
        Tick dt ->
            { model | clock = model.clock + dt }

        Click mouseX mouseY ->
            let
                ( dest_r, dest_theta ) =
                    toPolar
                        ( toFloat mouseX - toFloat model.w / 2
                        , toFloat model.h / 2 - toFloat mouseY
                        )

                theta =
                    retarget model.clock dest_theta model.theta |> speed 0.002 |> normalizeAngle

                r =
                    retarget model.clock (min armLength dest_r) model.r |> speed 0.4

                dur =
                    max (getDuration theta) (getDuration r)
            in
            { model | theta = theta |> duration dur, r = r |> duration dur }

        Resize width height ->
            { model | w = width, h = height }

        NoOp ->
            model


normalizeAngle anim =
    let
        from =
            getFrom anim

        to =
            getTo anim
    in
    if abs (from - to) < degrees 180 then
        anim

    else if to < from then
        normalizeAngle <| Animation.to (to + turns 1) anim

    else
        normalizeAngle <| Animation.to (to - turns 1) anim


armLength =
    400


scene : Model -> Element
scene { w, h, theta, r, clock } =
    let
        angle =
            animate clock theta

        radius =
            animate clock r

        base =
            Collage.circle 5 |> Collage.filled Color.charcoal

        arm =
            Collage.rect armLength 3 |> Collage.filled Color.charcoal |> Collage.moveX (armLength / 2)

        circle =
            Collage.circle 20 |> Collage.filled Color.red |> Collage.moveX radius

        group =
            Collage.group [ base, arm, circle ] |> Collage.rotate angle
    in
    Collage.collage w h [ group ]


subs : Sub Msg
subs =
    Sub.batch
        [ onResize Resize
        , onAnimationFrameDelta Tick
        , onClick mousePosition
        ]


mousePosition : Decoder Msg
mousePosition =
    Decode.map2 Click
        (Decode.field "pageX" Decode.int)
        (Decode.field "pageY" Decode.int)


main =
    program
        { init =
            ( model0
            , Task.perform
                (\{ viewport } ->
                    Resize (round viewport.width) (round viewport.height)
                )
                getViewport
            )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = always subs
        , view = scene >> Element.toHtml
        }
