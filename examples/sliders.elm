import Color
import Graphics.Element as E exposing (Element)
import Graphics.Input as I
import Time exposing (Time)
import Text
import Mouse

import Animation exposing (..)

{-| docs -}

type alias Model =
    { clock : Time
    , a1 : Animation
    , a2 : Animation
    , a3 : Animation
    , initial : Bool
    }

stay = static 0

model0 : Model
model0 = Model 0 stay stay stay True

type Action = Tick Time | Click

actions : Signal Action
actions =
    Signal.merge
        (Signal.map (always Click) Mouse.clicks)
        (Signal.map Tick (Time.fps 50))

slideLen = 500

update : Action -> Model -> Model
update action model =
    case action of
        Tick dt -> {model| clock = model.clock + dt}
        Click -> if model.initial
                 then let a = animation model.clock |> from 0 |> to slideLen |> duration (1.2 * Time.second)
                      in {model| a1 = a, a2 = a, a3 = a, initial = False }
                 else let t = model.clock
                          setDur = duration (750 * Time.millisecond)
                          dest = getFrom model.a1
                      in {model| a1 = animation t |> from (getTo model.a1) |> to dest |> setDur,
                                 a2 = animation t |> from (animate t model.a2) |> to dest |> setDur,
                                 a3 = retarget t dest model.a3 |> setDur
                         }

model : Signal Model
model = Signal.foldp update model0 actions


render : Model -> Element
render model =
    let w1 = animate model.clock model.a1
        w2 = animate model.clock model.a2
        w3 = animate model.clock model.a3
        h = 50
        text s = Text.fromString s |> Text.color Color.charcoal |> E.leftAligned |> E.width (slideLen + 100)
        slider w = E.layers
            [ E.spacer (slideLen + h) h |> E.color Color.lightGray
            , E.spacer (round w) h `E.beside` E.color Color.blue (E.spacer h h)
            ]
        padding = E.spacer 1 20
    in E.spacer 40 1 `E.beside` E.flow E.down
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

main = Signal.map render model
--main = Signal.map E.show model
