module Sliders exposing (main)

import Animation exposing (..)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick)
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as Decode exposing (Value)
import Svg exposing (Svg)
import Svg.Attributes as SA


type alias Model =
    { clock : Clock
    , a1 : Animation
    , a2 : Animation
    , a3 : Animation
    , initial : Bool
    }


second : Float
second =
    1000


millisecond : Float
millisecond =
    1


stay =
    static 0


model0 : Model
model0 =
    Model 0 stay stay stay True


type Msg
    = Tick Float
    | Click


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onClick (Decode.succeed Click)
        , onAnimationFrameDelta Tick
        ]


slideLen =
    500


update : Msg -> Model -> Model
update action model =
    case action of
        Tick dt ->
            { model | clock = model.clock + dt }

        Click ->
            if model.initial then
                let
                    a =
                        animation model.clock |> from 0 |> to slideLen |> duration (1.2 * second)
                in
                { model | a1 = a, a2 = a, a3 = a, initial = False }

            else
                let
                    t =
                        model.clock

                    setDur =
                        duration (750 * millisecond)

                    dest =
                        getFrom model.a1
                in
                { model
                    | a1 = animation t |> from (getTo model.a1) |> to dest |> setDur
                    , a2 = animation t |> from (animate t model.a2) |> to dest |> setDur
                    , a3 = retarget t dest model.a3 |> setDur
                }


view : Model -> Svg Msg
view model =
    let
        w1 =
            animate model.clock model.a1

        w2 =
            animate model.clock model.a2

        w3 =
            animate model.clock model.a3

        h =
            50

        text s =
            Html.p
                [ HA.style "color" "darkgray" ]
                [ Html.text s ]

        slider w =
            Svg.svg
                [ SA.style "display:block;"
                , SA.width (String.fromInt (slideLen + h))
                , SA.height (String.fromFloat h)
                ]
                [ Svg.rect
                    [ SA.width (String.fromFloat (slideLen + h))
                    , SA.height (String.fromFloat h)
                    , SA.fill "lightgray"
                    ]
                    []
                , Svg.rect
                    [ SA.width (String.fromFloat h)
                    , SA.height (String.fromFloat h)
                    , SA.x (String.fromFloat w)
                    , SA.fill "red"
                    ]
                    []
                ]
    in
    Html.div
        [ HA.style "margin" "20px" ]
        [ text "This is a demo of three different approaches to interrupted animation. Click the mouse rapidly."
        , text "The first slider is very naive. When interrupted, it pretends the previous animation has already completed, and jumps to the other side only to return. Astoundingly, this is how CSS transitions still work."
        , slider w1
        , text "This slider will undo the current animation, instantly reversing its direction."
        , slider w2
        , text "This slider will smoothly decelerate and reverse."
        , slider w3
        , text "Notice that all sliders reach their destination at the same time. The first slider is discontinuous is position; the second slider is discontinuous in velocity; the third slider is smooth."
        ]


main : Program Value Model Msg
main =
    Browser.element
        { init = always ( model0, Cmd.none )
        , update =
            \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }
