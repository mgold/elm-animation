{-
This demo shows the default behavior of retarget regarding the new speed/duration. An older version had bugs where
multiple retargets would cause the animation to overshoot by an arbitrary amount, and it would also become slow and
unresponsive. After contemplating what kind of complex algorithm could find the optimal duration, considering the
current velocity, direction of current vs. desired travel, timeRemaining, etc, I landed on the idea of retaining the old
speed. This is very simple, you just have to be sure to convert from duration if that's how it's specified, and works
remarkably well in terms of keeping the animation responsive, smooth, and not overshooting more than slightly.

However, if the new destination is very close to the current position, the animation is so short that it seems to stop
instantaneously. One could convert the average speed to a duration and provide a minimum, perhaps half a second. I tried
this and it didnt' behave naturally, and besides I don't like hard-coding a minimum time for a duration. Let the client
do that; I provide a sensible default.

-}
import Color
import Graphics.Collage as Collage
import Graphics.Element exposing (Element)
import Time exposing (Time)
import Window
import Mouse
import Debug

import Animation exposing (..)

type alias Model = { x : Animation, clock : Time, dots : List (Float, Time) }
model0 = Model (animation 0 |> duration 4000) 0 []

type Action = Tick Time | Click Float | Cull

actions : Signal Action
actions =
  Signal.mergeMany
    [ Signal.map Tick (Time.fps 20)
    , Signal.map2 (\w x -> toFloat x / toFloat w |> Click) Window.width Mouse.x |> Signal.sampleOn Mouse.clicks
    , Signal.map (always Cull) (Time.fps 0.5)
    ]

step : Action -> Model -> Model
step act model =
  case act of
    Tick dt ->
      let clock = model.clock + dt
          pos = animate clock model.x
      in {model | dots = (pos, clock) :: model.dots, clock = clock}

    Click xPos ->
      let _ = Debug.log "xPos" xPos in
      {model | x = retarget model.clock xPos model.x}

    Cull ->
      {model | dots = List.take 200 model.dots}

model : Signal Model
model = Signal.foldp step model0 actions

render : (Int, Int) -> Model -> Element
render (w, h) model =
    let baseY = toFloat h / -3
        toX x = toFloat w * x - toFloat (w//2)
        line = Collage.rect (toFloat w) 1 |> Collage.filled Color.gray |> Collage.moveY baseY
        dest = Collage.square 6 |> Collage.filled Color.red |> Collage.move (toX <| getTo model.x, baseY)
        circle = Collage.circle 8 |> Collage.filled Color.purple
                    |> Collage.move (toX <| animate model.clock model.x, baseY)
        dots = List.map (\(x, t) -> Collage.circle 3 |> Collage.filled Color.lightPurple
                    |> Collage.moveX (toFloat w * x - toFloat (w//2))
                    |> Collage.moveY (baseY - (t - model.clock)/10)) model.dots
    in Collage.collage w h <| line :: dest :: circle :: dots

main = Signal.map2 render Window.dimensions model
