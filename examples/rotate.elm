{- This example shows animating to an angle. The only slightly tricky bit is making sure you go the shortest way around.
-}
import Color
import Graphics.Collage as Collage
import Graphics.Element exposing (Element)
import Time exposing (Time)
import Window
import Mouse

import Animation exposing (..)

type alias Model = { theta : Animation, clock : Time }
model0 = Model (static 0) 0

mouseLocation : Signal (Float, Float)
mouseLocation =
    Signal.map2 (\(w,h) (x,y) -> toPolar (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y))
        Window.dimensions Mouse.position

type Action = Tick Time | Click (Float, Float)

actions : Signal Action
actions = Signal.merge
    (Signal.map Click (Signal.sampleOn Mouse.clicks mouseLocation))
    (Signal.map Tick (Time.fps 60))

step : Action -> Model -> Model
step act model = case act of
    Tick dt -> {model| clock <- model.clock + dt}
    Click (r, theta) -> {model| theta <- retarget model.clock theta model.theta |> speed 0.002 |> normalizeAngle}

normalizeAngle anim =
    let from = getFrom anim
        to = getTo anim
    in if | abs (from - to) < (degrees 180) -> anim
          | to < from -> Animation.to (to + turns 1) anim
          | to > from -> Animation.to (to - turns 1) anim

model : Signal Model
model = Signal.foldp step model0 actions

render : (Int, Int) -> Model -> Element
render (w, h) {theta, clock} =
    let angle = animate clock theta
        base = Collage.circle 5 |> Collage.filled Color.charcoal
        arm = Collage.rect 400 3 |> Collage.filled Color.charcoal |> Collage.moveX 200
        circle = Collage.circle 20 |> Collage.filled Color.red |> Collage.moveX 300
        group = Collage.group [base, arm, circle] |> Collage.rotate angle
    in Collage.collage w h [group]

main = Signal.map2 render Window.dimensions model
