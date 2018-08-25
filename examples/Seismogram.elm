module Seismogram exposing (main)

{-
   This demo shows the default behavior of retarget regarding the new
   speed/duration. An older version had bugs where multiple retargets
   would cause the animation to overshoot by an arbitrary amount, and
   it would also become slow and unresponsive. After contemplating
   what kind of complex algorithm could find the optimal duration,
   considering the current velocity, direction of current vs. desired
   travel, timeRemaining, etc, I landed on the idea of retaining the
   old speed. This is very simple, you just have to be sure to convert
   from duration if that’s how it's specified, and works remarkably
   well in terms of keeping the animation responsive, smooth, and not
   overshooting more than slightly.

   However, if the new destination is very close to the current position,
   the animation is so short that it seems to stop instantaneously. One
   could convert the average speed to a duration and provide a minimum,
   perhaps half a second. I tried this and it didn’t behave naturally,
   and besides I don’t like hard-coding a minimum time for a duration.
   Let the client do that; I provide a sensible default.
-}

import Animation exposing (..)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onClick, onResize)
import Json.Decode as Decode exposing (Decoder, Value)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task
import Time


second : Float
second =
    1000


type alias Model =
    { x : Animation
    , w : Int
    , h : Int
    , clock : Clock
    , dots : List ( Float, Float )
    }


model0 =
    Model (animation 0 |> duration 4000) 0 0 0 []


type Msg
    = Tick Float
    | Resize Int Int
    | Click Int
    | Cull


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onClick mouseXPosition
        , onResize Resize
        , Time.every (0.5 * second) (always Cull)
        ]


mouseXPosition : Decoder Msg
mouseXPosition =
    Decode.map Click
        (Decode.field "pageX" Decode.int)


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

        Resize w h ->
            { model | w = w, h = h }

        Click x ->
            let
                xPos =
                    toFloat x / toFloat model.w
            in
            { model | x = retarget model.clock xPos model.x }

        Cull ->
            { model | dots = List.take 200 model.dots }


view : Model -> Svg Msg
view model =
    let
        baseY =
            toFloat model.h * 2 / 3

        toX x =
            toFloat model.w * x

        line =
            Svg.line
                [ SA.x2 (String.fromInt model.w)
                , SA.stroke "gray"
                , SA.strokeWidth "1"
                ]
                []

        dest =
            Svg.rect
                [ SA.width "6"
                , SA.height "6"
                , SA.fill "red"
                , SA.x (String.fromFloat (-3 + toX (getTo model.x)))
                , SA.y "-3"
                ]
                []

        circle =
            Svg.circle
                [ SA.r "8"
                , SA.fill "purple"
                , SA.cx (String.fromFloat (toX <| animate model.clock model.x))
                ]
                []

        dots =
            List.map
                (\( x, t ) ->
                    Svg.circle
                        [ SA.r "3"
                        , SA.fill "mediumpurple"
                        , SA.cx (String.fromFloat (toFloat model.w * x))
                        , SA.cy (String.fromFloat ((t - model.clock) / 10))
                        ]
                        []
                )
                model.dots
    in
    Svg.svg
        [ SA.style "position:absolute;left:0;top:0"
        , SA.width (String.fromInt model.w)
        , SA.height (String.fromInt model.h)
        ]
        [ Svg.g
            [ SA.transform ("translate(0 " ++ String.fromFloat baseY ++ ")") ]
            (line :: dest :: circle :: dots)
        ]


main : Program Value Model Msg
main =
    Browser.element
        { init =
            always
                ( model0
                , Task.perform
                    (\{ viewport } ->
                        Resize (round viewport.width) (round viewport.height)
                    )
                    getViewport
                )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }
