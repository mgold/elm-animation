{- In this example, rather than steadily counting time and replacing animations, we rock time back and forth and
   maintain the same animations. A naive implementation, shown as the top panel, reverses direction immediately, rather
   than slowing down first. A more advanced implementation animates time itself: rather than always adding or
   subtracting the timestep, it interpolates between a factor or -1 and 1. (It only does this if the animation is
   interrupted; if it is restarted after completing it works identitcally to the naive version, to avoid lag at the
   start.)

   Click the mouse to trigger the animation. If you cick rapidly, the second panel looks a lot better.
-}


module Main exposing (..)

import Color exposing (Color)
import Element as E exposing (Element)
import Task
import Time exposing (Time, second)
import Mouse
import Window
import Html exposing (program)
import AnimationFrame
import Animation exposing (..)


width =
    animation 0 |> from 10 |> to 300 |> duration (0.67 * second)


height =
    animation 0 |> from 10 |> to 200 |> duration (0.67 * second) |> delay (0.33 * second)


color =
    animation 0 |> duration second


type alias Model =
    { trueClock : Time
    , arisClock :
        Time
        -- Aristotelian and Newtonian clocks
    , newtClock : Time
    , forward : Bool
    , newtFactor : Animation
    , windowSize : Window.Size
    }


model0 : Model
model0 =
    Model 0 0 0 False (static -1) (Window.Size 0 0)


type Msg
    = Tick Time
    | Resize Window.Size
    | Click
    | NoOp


subs : Sub Msg
subs =
    Sub.batch
        [ Mouse.clicks (always Click)
        , AnimationFrame.diffs Tick
        ]


animateRev =
    animation 0 |> from 1 |> to -1 |> duration (0.2 * second)


animateFwd =
    animation 0 |> from -1 |> to 1 |> duration (0.2 * second)


update : Msg -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        Resize size ->
            { model | windowSize = size }

        Tick dt ->
            let
                newTrueClock =
                    model.trueClock + dt
            in
                { model
                    | trueClock = newTrueClock
                    , arisClock =
                        model.arisClock
                            + dt
                            * (if model.forward then
                                1
                               else
                                -1
                              )
                        -- always 1 or -1
                    , newtClock =
                        model.newtClock + dt * (animate newTrueClock model.newtFactor)
                        -- often between 1 or -1
                }

        Click ->
            if model.forward then
                { model
                    | forward = False
                    , newtFactor =
                        if
                            model.newtClock > second
                            -- skip tweening if restarting animation from rest
                        then
                            static -1
                        else
                            animateRev |> delay model.trueClock
                    , arisClock =
                        min model.arisClock second
                        -- reset clocks that have gotten really big
                    , newtClock =
                        min model.newtClock second
                        -- but keep them if we're still animating
                }
            else
                { model
                    | forward =
                        True
                        -- works exactly opposite the other case
                    , newtFactor =
                        if model.newtClock < 0 then
                            static 1
                        else
                            animateFwd |> delay model.trueClock
                    , arisClock = max model.arisClock 0
                    , newtClock = max model.newtClock 0
                }


lerp from to v =
    from + (to - from) * v


lerp_ from to v =
    round (lerp (toFloat from) (toFloat to) v)


colorEase from to v =
    let
        ( rgb1, rgb2 ) =
            ( Color.toRgb from, Color.toRgb to )

        ( r1, g1, b1, a1 ) =
            ( rgb1.red, rgb1.green, rgb1.blue, rgb1.alpha )

        ( r2, g2, b2, a2 ) =
            ( rgb2.red, rgb2.green, rgb2.blue, rgb2.alpha )
    in
        Color.rgba (lerp_ r1 r2 v) (lerp_ g1 g2 v) (lerp_ b1 b2 v) (lerp a1 a2 v)


easeColor : Float -> Color
easeColor =
    colorEase Color.purple (Color.rgb 74 178 182)


padding : Element
padding =
    E.spacer 50 50


render : Time -> Element
render clock =
    let
        wid =
            animate clock width |> round

        hei =
            animate clock height |> round

        clr =
            animate clock color |> easeColor
    in
        E.spacer wid hei |> E.color clr


scene : Model -> Element
scene { arisClock, newtClock } =
    E.beside padding <|
        E.flow
            E.down
            [ padding
            , render arisClock
            , E.spacer 1 <| round <| getTo height - animate arisClock height
              -- keep top of second panel fixed
            , padding
            , render newtClock
            ]


main =
    program
        { init = ( model0, Task.perform Resize Window.size )
        , update = (\msg model -> ( update msg model, Cmd.none ))
        , subscriptions = always subs
        , view = scene >> E.toHtml
        }
