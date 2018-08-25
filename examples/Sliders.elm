module Sliders exposing (main)

import Animation exposing (..)
import Browser.Events exposing (onAnimationFrameDelta, onClick)
import Color
import Element as E exposing (Element)
import Html exposing (program)
import Json.Decode as Decode
import Text


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


subs : Sub Msg
subs =
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


render : Model -> Element
render model =
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
            Text.fromString s |> Text.color Color.charcoal |> E.leftAligned

        slider w =
            E.layers
                [ E.spacer (slideLen + h) h |> E.color Color.lightGray
                , E.beside (E.spacer (round w) h) <| E.color Color.blue (E.spacer h h)
                ]

        padding =
            E.spacer 1 20
    in
    E.beside (E.spacer 40 1) <|
        E.flow
            E.down
            [ text "This is a demo of three different approaches to interrupted animation. Click the mouse rapidly."
            , padding
            , text "The first slider is very naive. When interrupted, it pretends the previous animation has already completed, and jumps to the other side only to return. Astoundingly, this is how CSS transitions still work."
            , slider w1
            , padding
            , text "This slider will undo the current animation, instantly reversing its direction."
            , slider w2
            , padding
            , text "This slider will smoothly decelerate and reverse."
            , slider w3
            , text "Notice that all sliders reach their destination at the same time. The first slider is discontinuous is position; the second slider is discontinuous in velocity; the third slider is smooth."
            ]


main =
    program
        { init = ( model0, Cmd.none )
        , update =
            \msg model -> ( update msg model, Cmd.none )
        , subscriptions = always subs
        , view = render >> E.toHtml
        }
