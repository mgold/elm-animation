module Main exposing (..)

{-| Button menu example. Implements the essential visual form implemented withn React Motion here:
https://medium.com/@nashvail/a-gentle-introduction-to-react-motion-dc50dd9f2459
Uses half as many lines, but the devil is in the details, and this isn't a usable component itself.
-}

import Color exposing (lightGray, darkGray, lightBlue, white)
import Collage
import Element exposing (Element)
import Time exposing (Time)
import Task
import Window
import Mouse
import Html exposing (program)
import AnimationFrame
import Animation exposing (..)


mainButtonRad =
    45


childButtonRad =
    24


numChildren =
    5


flyOutRad =
    130


separationAngle =
    degrees 40


fanAngle =
    (numChildren - 1) * separationAngle


baseAngle =
    (pi - fanAngle) / 2


type Msg
    = Tick Time
    | Click Mouse.Position
    | Resize Window.Size
    | NoOp


type alias Model =
    { w : Int
    , h : Int
    , theta : Animation
    , rs : List Animation
    , clock : Time
    , open : Bool
    }


delayed i =
    delay (toFloat (numChildren - i) * 50)


model0 =
    let
        theta =
            animation -1000 |> from (degrees 45) |> to 0 |> duration 100

        rs =
            List.map (\i -> theta |> from flyOutRad |> delayed i) (List.range 0 (numChildren - 1))
    in
        Model 0 0 theta rs 0 False


update : Msg -> Model -> Model
update act model =
    case act of
        Tick dt ->
            { model | clock = model.clock + dt }

        Click rawPos ->
            let
                x =
                    toFloat rawPos.x - toFloat model.w / 2

                y =
                    toFloat model.h / 2 - toFloat rawPos.y
            in
                if x ^ 2 + y ^ 2 < mainButtonRad ^ 2 then
                    { model
                        | open = not model.open
                        , theta = undo model.clock model.theta
                        , rs = List.indexedMap (\i -> undo model.clock >> delayed i) model.rs
                    }
                else
                    model

        Resize { width, height } ->
            { model | w = width, h = height }

        NoOp ->
            model


scene : Model -> Element
scene { w, h, rs, theta, clock } =
    let
        bg =
            Collage.rect (toFloat w) (toFloat h) |> Collage.filled lightGray

        angle =
            animate clock theta

        rect =
            Collage.rect (mainButtonRad / 5) (mainButtonRad / 1.25) |> Collage.filled white

        circle =
            Collage.circle mainButtonRad |> Collage.filled lightBlue

        group =
            Collage.group [ circle, rect, rect |> Collage.rotate (degrees 90) ]
                |> Collage.rotate angle

        child func i a =
            Collage.circle childButtonRad
                |> func
                |> Collage.move (fromPolar ( animate clock a, baseAngle + separationAngle * toFloat i ))

        stroked =
            Collage.outlined (Collage.solid darkGray)

        filled =
            Collage.filled white

        children =
            List.indexedMap (child stroked) rs ++ List.indexedMap (child filled) rs
    in
        Collage.collage w h (bg :: children ++ [ group ])


subs : Sub Msg
subs =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.diffs Tick
        , Mouse.clicks Click
        ]


main =
    program
        { init = ( model0, Task.perform Resize Window.size )
        , update = (\msg model -> ( update msg model, Cmd.none ))
        , subscriptions = always subs
        , view = scene >> Element.toHtml
        }
