module Rotate exposing (main)

{- This example shows animating to an angle.
   The only slightly tricky bit is making sure
   you go the shortest way around.
-}

import Animation exposing (..)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onClick, onResize)
import Json.Decode as Decode exposing (Decoder, Value)
import Svg exposing (Svg)
import Svg.Attributes as SA
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
                        , toFloat mouseY - toFloat model.h / 2
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


view : Model -> Svg Msg
view { w, h, theta, r, clock } =
    let
        angle =
            animate clock theta

        radius =
            animate clock r

        base =
            Svg.circle [ SA.r "5", SA.fill "gray" ] []

        arm =
            Svg.line
                [ SA.x2 (String.fromInt armLength)
                , SA.strokeWidth "3"
                , SA.stroke "gray"
                ]
                []

        circle =
            Svg.circle
                [ SA.r "20"
                , SA.fill "red"
                , SA.cx (String.fromFloat radius)
                ]
                []

        group =
            Svg.g
                [ SA.transform
                    ("translate("
                        ++ String.fromInt (w // 2)
                        ++ " "
                        ++ String.fromInt (h // 2)
                        ++ ") rotate("
                        ++ String.fromFloat (angle / pi * 180)
                        ++ ")"
                    )
                ]
                [ base, arm, circle ]
    in
    Svg.svg
        [ SA.style "position:absolute;left:0;top:0"
        , SA.width (String.fromInt w)
        , SA.height (String.fromInt h)
        ]
        [ group ]


subscriptions : Model -> Sub Msg
subscriptions _ =
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
