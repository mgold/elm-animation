module ButtonMenu exposing (main)

{- Button menu example. Implements the essential visual
   form implemented with [React Motion here](https://medium.com/@nashvail/a-gentle-introduction-to-react-motion-dc50dd9f2459)
-}

import Animation exposing (..)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onClick, onResize)
import Json.Decode as Decode exposing (Decoder, Value)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task


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
    = Tick Float
    | Click Int Int
    | Resize Int Int
    | NoOp


type alias Model =
    { w : Int
    , h : Int
    , theta : Animation
    , rs : List Animation
    , clock : Clock
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

        Click mouseX mouseY ->
            let
                x =
                    toFloat mouseX - toFloat model.w / 2

                y =
                    toFloat model.h / 2 - toFloat mouseY
            in
            if x ^ 2 + y ^ 2 < mainButtonRad ^ 2 then
                { model
                    | open = not model.open
                    , theta = undo model.clock model.theta
                    , rs = List.indexedMap (\i -> undo model.clock >> delayed i) model.rs
                }

            else
                model

        Resize width height ->
            { model | w = width, h = height }

        NoOp ->
            model


view : Model -> Svg Msg
view { w, h, rs, theta, clock } =
    let
        bg =
            Svg.rect
                [ SA.width (String.fromInt w)
                , SA.height (String.fromInt h)
                , SA.fill "lightgray"
                ]
                []

        angle =
            animate clock theta

        rect =
            Svg.rect
                [ SA.width (String.fromFloat (mainButtonRad / 5))
                , SA.height (String.fromFloat (mainButtonRad / 1.25))
                , SA.x (String.fromFloat (-mainButtonRad / 10))
                , SA.y (String.fromFloat (-mainButtonRad / 2.5))
                , SA.fill "white"
                ]
                []

        circle =
            Svg.circle
                [ SA.r (String.fromFloat mainButtonRad)
                , SA.fill "blue"
                ]
                []

        group =
            Svg.g [ SA.transform ("rotate(" ++ String.fromFloat (angle / pi * 180) ++ ")") ]
                [ circle
                , rect
                , Svg.g [ SA.transform "rotate(90)" ] [ rect ]
                ]

        child isFilled i a =
            let
                ( x, y ) =
                    fromPolar
                        ( animate clock a
                        , -baseAngle - separationAngle * toFloat i
                        )
            in
            Svg.circle
                [ SA.r (String.fromFloat childButtonRad)
                , SA.cx (String.fromFloat x)
                , SA.cy (String.fromFloat y)
                , SA.fill "white"
                , SA.stroke "darkgray"
                , SA.strokeWidth "1"
                ]
                []
    in
    Svg.svg
        [ SA.style "position:absolute;left:0;top:0"
        , SA.width (String.fromInt w)
        , SA.height (String.fromInt h)
        ]
        [ bg
        , Svg.g
            [ SA.transform
                ("translate("
                    ++ String.fromInt (w // 2)
                    ++ ","
                    ++ String.fromInt (h // 2)
                    ++ ")"
                )
            ]
            (List.indexedMap (child False) rs
                ++ [ group ]
            )
        ]


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
        , subscriptions = always subs
        , view = view
        }
